package com.aivean.isorpg.game

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import akka.event.LoggingReceive
import com.aivean.isorpg.routes.Client
import xitrum.Config

import scala.collection.immutable.ListMap
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

    val worldSize = 100
    var world =   TerrainGen.apply(worldSize)
    var chunks = world.groupBy(_._1.chunk)

    var players = Seq[PlayerView]()

    def freeToMoveTo(p:Point) =
      world.get(p.down).exists(_.standable) &&
      !world.get(p).exists(!_.passable) &&
      !players.exists(_.state.p == p)

    def genFreePoint = {
      val p = Random.shuffle(players.map(_.state.p)).headOption.getOrElse(Random.shuffle(world.keys.toSeq).head)

      Random.shuffle(world.keys.toSeq.filter(_.distTo(p) <= 10).map(_.up).filter(freeToMoveTo)).head
    }

    def receive = LoggingReceive {
      case ClientConnected(uuid) =>
        val client = sender
        val pos = genFreePoint
        val newPlayerActor = context.actorOf(Player.props(pos, client))

        players.foreach {_.actor ! Player.PlayerAdded(uuid, pos)}

        players :+= new PlayerView(uuid, Movement.Standing(pos), newPlayerActor)

        players.foreach { p =>
          client ! Client.PlayerAdded(p.uuid, p.state.p)
        }

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
        players.find(_.actor == sender).filter(_.state.isInstanceOf[Movement.Standing])
          .foreach { movingPlayer =>

            log.debug(s"found player $movingPlayer ${movingPlayer.state}")

            val path = Utils.path(movingPlayer.state.p, targetP, { (p,p1)=> freeToMoveTo(p1)})

            log.debug(s"found path $path")

            path.flatMap(_.slice(1, 2).headOption).foreach { nextPoint =>
              log.debug(s"moving to next point $nextPoint")
              val stepTime = 400 milliseconds

              val ts = System.currentTimeMillis() + stepTime.toMillis
              movingPlayer.state = Movement.MovingTo(nextPoint, ts)

              players.foreach(_.actor ! Player.PlayerMoved(movingPlayer.uuid, ts, nextPoint))

              context.system.scheduler.scheduleOnce(stepTime, self,
                UpdatePlayersMovement(movingPlayer.actor))
            }
          }

      case msg@UpdatePlayersMovement(playerActor) =>
        players.find(_.actor == playerActor).foreach { player =>
          log.debug("updating moving state")
          val time = System.currentTimeMillis()
          player.state match {
            case Movement.MovingTo(p, ts) if ts <= time =>
              player.state = Movement.Standing(p)
              log.debug(s"arrived at $p")
              player.actor ! Player.ArrivedAt(p)
            case Movement.MovingTo(p, ts) =>
              log.debug(s"moving to $p, not yet arrived")
              context.system.scheduler.scheduleOnce((ts - time) milliseconds, self, msg)
            case _ =>
          }
        }

      case ChunksRequest(chIds) =>
        sender ! PlayerChunksController.TilesAdded(
          chIds.toList.flatMap(id => chunks.get(id).map(c => (id, c))).toMap)

      //TODO delegate broadcast functionality to other dedicated actor

      case PlayerTalking(msg) => players.find(_.actor == sender).foreach { player =>
        players.foreach(_.actor ! Player.PlayerTalking(player.uuid, msg))
      }

      case ClientDisconnected =>
        players.find(_.actor == sender).foreach { p =>
          players = players.filterNot(_ == p)
          players.foreach(_.actor ! Player.PlayerRemoved(p.uuid))
        }
    }
  }

  object World {
    //def props = Props(new World)
    val world = Config.actorSystem.actorOf(Props(new World))

    case class ClientConnected(uuid: String)

    case object ClientDisconnected

    case class AdminCommand(cmd:String, p:ActorRef)

    case class PlayerWantsToMove(p: Point)

    case class UpdatePlayersMovement(player:ActorRef)

    case class PlayerTalking(msg:String)

    case class ChunksRequest(ids:Set[Long])

    class PlayerView(
                      val uuid: String,
                      var state: Movement.Type,
                      val actor: ActorRef
                    )

    object Movement {
      sealed trait Type {
        def p:Point
      }

      case class Standing(p: Point) extends Type

      case class MovingTo(p: Point, arrivalTs: Long) extends Type

    }
  }
