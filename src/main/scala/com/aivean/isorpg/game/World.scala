package com.aivean.isorpg.game

import akka.actor.{ActorLogging, Actor, ActorRef, Props}
import akka.event.LoggingReceive
import com.aivean.isorpg.routes.Client
import xitrum.Config

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

    var world =   TerrainGen.apply(100)
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
        val newPlayerActor = context.actorOf(Player.props)

        client ! Client.PlayerBound(newPlayerActor)

        players.foreach { player =>
          player.client ! Client.PlayerAdded(uuid, pos)
        }

        players :+= new PlayerView(uuid, Movement.Standing(pos), newPlayerActor, client, Set.empty)
        self ! UpdatePlayersChunks(newPlayerActor)

        players.foreach { p =>
          client ! Client.PlayerAdded(p.uuid, p.state.p)
        }

      case PlayerWantsToMove(targetP) =>
        log.debug(s"want to move to $targetP")
        players.find(_.player == sender).filter(_.state.isInstanceOf[Movement.Standing])
          .foreach { movingPlayer =>

            log.debug(s"found player $movingPlayer ${movingPlayer.state}")

            val path = Utils.path(movingPlayer.state.p, targetP, { (p,p1)=> freeToMoveTo(p1)})

            log.debug(s"found path $path")

            path.flatMap(_.slice(1, 2).headOption).foreach { nextPoint =>
              log.debug(s"moving to next point $nextPoint")
              val stepTime = 400 milliseconds

              val ts = System.currentTimeMillis() + stepTime.toMillis
              movingPlayer.state = Movement.MovingTo(nextPoint, ts)

              players.map(_.client)//.filterNot(movingPlayer.client ==)
                .foreach(_ ! Client.MoveTo(movingPlayer.uuid, ts, nextPoint))

              context.system.scheduler.scheduleOnce(stepTime, self,
                UpdatePlayersMovement(movingPlayer.player))
            }
          }

      case msg@UpdatePlayersMovement(playerActor) =>
        players.find(_.player == playerActor).foreach { player =>
          log.debug("updatign moving state")
          val time = System.currentTimeMillis()
          player.state match {
            case Movement.MovingTo(p, ts) if ts <= time =>
              player.state = Movement.Standing(p)
              log.debug(s"arrived at $p")
              self ! UpdatePlayersChunks(playerActor)
              player.player ! Player.ArrivedAt(p)
            case Movement.MovingTo(p, ts) =>
              log.debug(s"moving to $p, not yet arrived")
              context.system.scheduler.scheduleOnce((ts - time) milliseconds, self, msg)
            case _ =>
          }
        }

      case UpdatePlayersChunks(playerActor) =>
        players.find(_.player == playerActor).foreach { player =>
          val neededChunks = {
            // 1/2 tile
            val (t0, t1) = (math.cos(math.Pi / 6), math.sin(math.Pi / 6))
            val yShift = -player.state.p.z
            for (x <- -14 to 14; y <- (-12 + yShift) to 12)
              yield player.state.p.moved(
                (x / (2 * t0) + y / (2 * t1)).toInt,
                (-(x / (2 * t0)) + y / (2 * t1)).toInt
              ).chunk

          }.toSet.filter(chunks.contains)

          // toAdd
          for (chId <- neededChunks.diff(player.chunks); tiles = chunks(chId)) {
            player.client ! Client.TilesAdded(chId,
              tiles.toList.map { case (p, t) => Client.TileStub(t.tile, t.standable, t.overlay, p) })
          }

          // toRemove
          player.chunks.diff(neededChunks).toList match {
            case Nil =>
            case x => player.client ! Client.TilesRemoved(x)
          }

          player.chunks = neededChunks
        }

      case PlayerTalking(msg) => players.find(_.player == sender).foreach { player =>
        players.map(_.client).foreach(_ ! Client.PlayerTalking(player.uuid, msg))
      }

      case ClientDisconnected =>
        players.find(_.player == sender).foreach { p =>
          players.map(_.client).foreach(_ ! Client.PlayerRemoved(p.uuid))
          players = players.filterNot(_ == p)
        }
    }
  }

  object World {
    //def props = Props(new World)
    val world = Config.actorSystem.actorOf(Props(new World))

    case class ClientConnected(uuid: String)

    case object ClientDisconnected

    case class PlayerWantsToMove(p: Point)

    case class UpdatePlayersChunks(player:ActorRef)

    case class UpdatePlayersMovement(player:ActorRef)

    case class PlayerTalking(msg:String)

    class PlayerView(
                      val uuid: String,
                      var state: Movement.Type,
                      val player: ActorRef,
                      val client: ActorRef,
                      var chunks:Set[Long]
                    )

    object Movement {
      sealed trait Type {
        def p:Point
      }

      case class Standing(p: Point) extends Type

      case class MovingTo(p: Point, arrivalTs: Long) extends Type

    }
  }
