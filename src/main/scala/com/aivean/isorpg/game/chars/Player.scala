package com.aivean.isorpg.game.chars

import akka.actor._
import akka.event.LoggingReceive
import com.aivean.isorpg.game.{Point, World}
import com.aivean.isorpg.routes.Client

import scala.language.postfixOps

/**
  *
  * @author <a href="mailto:ivan.zaytsev@webamg.com">Ivan Zaytsev</a>
  *         2015-11-13
  */

class Player private(override val initialPos: Point, client: ActorRef)
  extends Actor with ActorLogging with MovingObject {

  import Player._

  val chunksController = context.actorOf(PlayerChunksController.props(context.parent, client))

  client ! Client.PlayerBound(self)
  chunksController ! PlayerChunksController.UpdateChunks(pos)


  def receive = LoggingReceive(movingObjectBehavior.orElse[Any, Unit]({
    case PlayerAdded(uuid, p, sprite) => client ! Client.PlayerAdded(uuid, p, sprite)
    case PlayerMoved(uuid, ts, p) => client ! Client.PlayerMoved(uuid, ts, p)
    case PlayerTalking(uuid, msg) => client ! Client.PlayerTalking(uuid, msg)
    case PlayerRemoved(uuid) => client ! Client.PlayerRemoved(uuid)

    case RequestMoveTo(p) => setMovementTarget(p)

    case Say(msg) => context.parent ! World.PlayerTalking(msg)

    case ServerMessage(msg) => client ! Client.ServerMessage(msg)

    case Disconnected =>
      context.parent ! World.ClientDisconnected
      context.stop(self)
  }))

  override def onArrivedAt(): Unit = {
    chunksController ! PlayerChunksController.UpdateChunks(pos)
  }
}

object Player {
  def props(p: Point, client: ActorRef) = Props(new Player(p, client))

  case class RequestMoveTo(p: Point)

  case class Say(msg: String)

  case object Disconnected

  case class PlayerAdded(uuid: String, p: Point, sprite:String)

  case class PlayerMoved(uuid: String, ts: Long, p: Point)

  case class PlayerTalking(uuid: String, msg: String)

  case class PlayerRemoved(uuid: String)

  case class ServerMessage(msg: String)

}
