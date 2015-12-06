package com.aivean.isorpg.game

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import akka.event.LoggingReceive
import com.aivean.isorpg.game.Tiles.TileType
import com.aivean.isorpg.game.VisibilityController.CharCommons
import com.aivean.isorpg.game.chars.{Monster, MovingObject, Player, PlayerChunksController}
import xitrum.Config

import scala.collection.immutable.ListMap
import scala.collection.mutable
import scala.concurrent.duration._
import scala.language.postfixOps
import scala.util.Random

/**
  *
  * @author <a href="mailto:ivan.zaytsev@webamg.com">Ivan Zaytsev</a>
  *         2015-11-13
  */

  class World private extends Actor with ActorLogging {
    import World._
    implicit val ec = context.dispatcher

    val visContr = context.actorOf(VisibilityController.props, "visContrl")

    val worldSize = 100
    var world =   TerrainGen.apply(worldSize)
    var chunks = world.groupBy(_._1.chunk)

    var occupied = Set[Point]()
    var players = Seq[CharView]()
    var npcs = Seq[CharView]()
    def allChars = players.view ++ npcs

    for(i <- 1 to 100) {
      val fp = genFreePoint
      occupied += fp
      val uuid = Utils.uuid
      val a = context.actorOf(Monster.props(fp), "poring-" + uuid)
      val view = new CharView(uuid, Movement.Standing(fp), a, "poring")
      npcs :+= view
      visContr ! VisibilityController.NPCAdded(CharCommons(view.uuid, view.state.p, view.sprite))
    }

    def freeToMoveTo(p:Point) =
      world.get(p.down).exists(_.standable) &&
      !world.get(p).exists(!_.passable) &&
      !occupied.contains(p)

    def genFreePoint = {
      val p = Random.shuffle(players.map(_.state.p)).headOption.getOrElse(Random.shuffle(world.keys.toSeq).head)

      Random.shuffle(world.keys.toSeq.filter(_.distTo(p) <= 10).map(_.up).filter(freeToMoveTo)).head
    }

    def receive = LoggingReceive {
      case ClientConnected(uuid) =>
        val client = sender
        val char = {
          val pos = genFreePoint
          val newPlayerActor = context.actorOf(Player.props(pos, client), "player-" + uuid)
          new CharView(uuid, Movement.Standing(pos), newPlayerActor, "player")
        }
        occupied += char.state.p
        players :+= char

        visContr ! VisibilityController.PlayerAdded(CharCommons(char.uuid, char.state.p, char.sprite),
          char.actor, 25, 21)

      case AdminCommand(cmd, player) =>
        val NEIGHTBOURS_PATTERN = """tileNeighbors[^\d-]+(-?[\d]+)[^\d-]+(-?[\d]+)[^\d-]+(-?[\d]+)""".r
        cmd match {
        case "generateWorld" =>
          world = TerrainGen.apply(worldSize)
          chunks = world.groupBy(_._1.chunk)
          players.foreach(_.actor ! Player.ServerMessage("World Regenerated!"))
        case "location" =>
          players.find(_.actor == player) foreach { player =>
            player.actor ! Player.ServerMessage(player.state.p.toString)
          }
        case NEIGHTBOURS_PATTERN(x, y, z) =>
          try {
            val p = Point(x.toInt, y.toInt, z.toInt)
            val result = ListMap("point" -> p, "s" -> p.south, "sw" -> p.south.west, "w" -> p.west, "nw" -> p.north
              .west, "n" -> p.north, "ne" -> p.north.east, "e" -> p.east, "se" -> p.south.east, "up" -> p.up,
              "down" -> p.down)
            .mapValues(world.get(_).map(_.tile).getOrElse("")).map(x => x._1 + ":"+x._2).mkString(", ")
            player ! Player.ServerMessage(result)
          } catch {case e: Throwable => player ! Player.ServerMessage("Error: " + e.getMessage)}

        case x => player ! Player.ServerMessage("unknown command: " + x)
      }

      case PlayerWantsToMove(targetP) =>
        log.debug(s"want to move to $targetP")
        allChars.find(_.actor == sender).filter(_.state.isInstanceOf[Movement.Standing])
          .foreach { movingPlayer =>

            log.debug(s"found player $movingPlayer ${movingPlayer.state}")

            val path = Utils.path(movingPlayer.state.p, targetP, { (p,p1)=> freeToMoveTo(p1)})

            log.debug(s"found path $path")

            path.flatMap(_.slice(1, 2).headOption).foreach { nextPoint =>
              log.debug(s"moving to next point $nextPoint")
              val stepTime = 400 milliseconds

              val ts = System.currentTimeMillis() + stepTime.toMillis
              occupied -= movingPlayer.state.p
              movingPlayer.state = Movement.MovingTo(nextPoint, ts)
              occupied += movingPlayer.state.p

              visContr ! VisibilityController.CharMoving(movingPlayer.uuid, nextPoint, ts)

              context.system.scheduler.scheduleOnce(stepTime, self,
                UpdatePlayersMovement(movingPlayer.actor))
            }
          }

      case msg@UpdatePlayersMovement(playerActor) =>
        allChars.find(_.actor == playerActor).foreach { player =>
          log.debug("updating moving state")
          val time = System.currentTimeMillis()
          player.state match {
            case Movement.MovingTo(p, ts) if ts <= time =>
              occupied -= p
              player.state = Movement.Standing(p)
              occupied += p
              log.debug(s"arrived at $p")
              player.actor ! MovingObject.ArrivedAt(p)
              visContr ! VisibilityController.CharPositionChanged(player.uuid, p)
            case Movement.MovingTo(p, ts) =>
              log.debug(s"moving to $p, not yet arrived")
              context.system.scheduler.scheduleOnce((ts - time) milliseconds, self, msg)
            case _ =>
          }
        }

      case ChunksRequest(chIds) =>
        sender ! PlayerChunksController.TilesAdded(
          chIds.toList.flatMap(id => chunks.get(id).map(c => (id, c))).toMap)

      case SurroundingsRequest(point, dist) =>
        val q = mutable.Queue[Point](point)
        var res = Map[Point, TileType]()
        while (q.nonEmpty) {
          val p = q.dequeue()
          val toAdd = (p.adjFlatAll :+ p.up :+ p.down)
            .filterNot(res.contains).filter(_.distTo(point) <= dist)
            .flatMap(p => world.get(p).map(t => (p, t))).toMap
          res ++= toAdd
          toAdd.keys.foreach(q.enqueue(_))
        }
        sender ! Monster.SurroundingsView(res)

      //TODO delegate broadcast functionality to other dedicated actor

      case PlayerTalking(msg) => players.find(_.actor == sender).foreach { player =>
        players.foreach(_.actor ! Player.PlayerTalking(player.uuid, msg))
      }

      case ClientDisconnected =>
        players.find(_.actor == sender).foreach { p =>
          players = players.filterNot(_ == p)
          occupied -= p.state.p
          visContr ! VisibilityController.CharRemoved(p.uuid)
        }
    }
  }

  object World {
    //def props = Props(new World)
    val world = Config.actorSystem.actorOf(Props(new World), "world")

    case class ClientConnected(uuid: String)

    case object ClientDisconnected

    case class AdminCommand(cmd:String, p:ActorRef)

    case class PlayerWantsToMove(p: Point)

    case class UpdatePlayersMovement(player:ActorRef)

    case class PlayerTalking(msg:String)

    case class ChunksRequest(ids:Set[Long])

    case class SurroundingsRequest(p:Point, dist:Float)

    class CharView(
                      val uuid: String,
                      var state: Movement.Type,
                      val actor: ActorRef,
                      val sprite: String
                    )

    object Movement {
      sealed trait Type {
        def p:Point
      }

      case class Standing(p: Point) extends Type

      case class MovingTo(p: Point, arrivalTs: Long) extends Type
    }
  }
