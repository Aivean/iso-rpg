package com.aivean.isorpg.game

import akka.actor._
import akka.event.LoggingReceive
import com.aivean.isorpg.game.Tiles.TileType
import com.aivean.isorpg.routes.Client

import scala.concurrent.duration.DurationDouble
import scala.language.postfixOps

/**
  *
  * @author <a href="mailto:ivan.zaytsev@webamg.com">Ivan Zaytsev</a>
  *         2015-11-13
  */

class Player private (initialPos:Point, client:ActorRef) extends Actor with ActorLogging {

  import Player._

  var movingTarget: Option[Point] = None
  var movementUpdate: Option[Cancellable] = None
  var pos = initialPos

  val chunksController = context.actorOf(PlayerChunksController.props(context.parent, client))

  implicit val ec = context.dispatcher

  client ! Client.PlayerBound(self)
  chunksController ! PlayerChunksController.UpdateChunks(pos)

  def receive = LoggingReceive {
    case PlayerAdded(uuid, p) => client ! Client.PlayerAdded(uuid, p)
    case PlayerMoved(uuid, ts, p) => client ! Client.PlayerMoved(uuid, ts, p)
    case PlayerTalking(uuid, msg) => client ! Client.PlayerTalking(uuid, msg)
    case PlayerRemoved(uuid) => client ! Client.PlayerRemoved(uuid)

    case RequestMoveTo(p) =>
      movingTarget = Some(p)
      log.debug(s"set target $p")
      self ! UpdateMovement

    case UpdateMovement =>
      movingTarget.foreach { moveTo =>
        log.debug(s"updating movement $moveTo")
        context.parent ! World.PlayerWantsToMove(moveTo)
        movementUpdate = movementUpdate.orElse(
          Some(context.system.scheduler.schedule(0.5 seconds, 0.5 seconds, self, UpdateMovement)))
      }

    case ArrivedAt(p) =>
      pos = p
      chunksController ! PlayerChunksController.UpdateChunks(pos)
      movementUpdate.foreach(_.cancel())
      movementUpdate = None
      if (movingTarget.contains(p)) {
        movingTarget = None
      } else {
        self ! UpdateMovement
      }

    case Say(msg) => context.parent ! World.PlayerTalking(msg)

    case Disconnected =>
      context.parent ! World.ClientDisconnected
      context.stop(self)
  }
}

object Player {
  def props(p:Point, client:ActorRef) = Props(new Player(p, client))

  case class RequestMoveTo(p: Point)

  case class Say(msg: String)

  case class ArrivedAt(p: Point)

  case object UpdateMovement

  case object Disconnected

  case class PlayerAdded(uuid: String, p: Point)

  case class PlayerMoved(uuid: String, ts: Long, p: Point)

  case class PlayerTalking(uuid: String, msg:String)

  case class PlayerRemoved(uuid: String)
}



class PlayerChunksController private(world:ActorRef, client:ActorRef) extends Actor {
  import PlayerChunksController._

  var chunks = Set[Long]()

  def receive = {
    case TilesAdded(chunksMap) =>
      for((chId, tiles) <- chunksMap) {
        client ! Client.TilesAdded(chId,
          tiles.toList.map { case (p, t) => Client.TileStub(t.tile, t.standable, t.overlay, p) })
      }

    case UpdateChunks(pos)  =>
      val neededChunks = {
        // 1/2 tile
        val (t0, t1) = (math.cos(math.Pi / 6), math.sin(math.Pi / 6))
        val yShift = -pos.z
        for (x <- -14 to 14; y <- (-12 + yShift) to 12)
          yield pos.moved(
            (x / (2 * t0) + y / (2 * t1)).toInt,
            (-(x / (2 * t0)) + y / (2 * t1)).toInt
          ).chunk
      }.toSet

      // toAdd
      world ! World.ChunksRequest(neededChunks.diff(chunks))

      // toRemove
      chunks.diff(neededChunks).toList match {
        case Nil =>
        case x => client ! Client.TilesRemoved(x)
      }

      chunks = neededChunks
  }
}

object PlayerChunksController {
  def props(world:ActorRef, client:ActorRef) = Props(new PlayerChunksController(world, client))

  case class UpdateChunks(p:Point)

  case class TilesAdded(chunks:Map[Long, Map[Point, TileType]])
}