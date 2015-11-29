package com.aivean.isorpg.routes

import akka.actor.ActorRef
import com.aivean.isorpg.game.{World, Player, Point}
import org.json4s
import org.json4s.Extraction._
import org.json4s.JsonAST.{JString, JValue}
import org.json4s._
import org.json4s.jackson.JsonMethods
import xitrum._
import xitrum.annotation.WEBSOCKET

import scala.util.{Failure, Success, Try}

/**
  *
  * @author <a href="mailto:ivan.zaytsev@webamg.com">Ivan Zaytsev</a>
  *         2015-11-26
  */
@WEBSOCKET("stream")
class Client extends WebSocketAction  {
  import Client._
  import org.json4s.JsonDSL._

  implicit val formats = DefaultFormats + FieldSerializer[Point]()

  var player: Option[ActorRef] = None

  val uuid = java.util.UUID.randomUUID.toString

  def send(v:JValue) = {
    respondWebSocketText(JsonMethods.compact(v))
  }

  def execute(): Unit = {
    log.debug("onOpen")

    World.world ! World.ClientConnected(uuid)

    context.become {

      case WebSocketText(text) =>
        log.info("onTextMessage: " + text)

        Try(JsonMethods.parse(text)) match {
          case Success(json) =>
            if (json \ "t" == json4s.JString("ping")) {
              send(json merge (("t" -> "pong") ~ ("serverTs" -> System.currentTimeMillis())))
            } else player.foreach { p =>
              json \ "t" match {
                case JString("m") => p ! Player.RequestMoveTo(json.extract[Point])
                case JString("say") => (json \ "msg").extractOpt[String].foreach {
                  msg => p ! Player.Say(msg)
                }
                case JString("admin") => (json \ "cmd").extractOpt[String].foreach {
                  cmd => World.world ! World.AdminCommand(cmd, p)
                }

                case x =>
                  log.warn("unrecognized message: " + x)
              }
            }

          case Failure(f) => log.warn("Can't parse", f)
        }

      case PlayerBound(p) =>
        log.info("Player bound! " + uuid)
        this.player = Some(p)

      case PlayerMoved(id, ts, p) =>
        send(
          ("t" -> "m") ~
            ("id" -> id) ~
            ("ts" -> ts) merge decompose(p)
        )

      case PlayerAdded(id, p) =>
        send(
          ("t" -> "pa") ~
            ("id" -> id) ~
            ("cur" -> (id == uuid)) merge decompose(p)
        )

      case TilesAdded(chunkId, tiles) =>
        send(("t" -> "ta") ~
          ("c" -> chunkId) ~
          ("tiles" ->
            tiles.map {
              case TileStub(tile, standable, overlay, p) =>
                (("tile" -> tile) ~
                  ("standable" -> standable) ~
                  ("overlay" -> overlay)) merge decompose(p)
            }))

      case TilesRemoved(chunks) =>
        send(("t" -> "tr") ~
          ("c" -> chunks)
        )

      case PlayerRemoved(id) =>
        send(
          ("t" -> "pr") ~
            ("id" -> id)
        )

      case PlayerTalking(uuid, msg) =>
        send(
          ("t" -> "pt") ~
            ("id" -> uuid) ~
            ("msg" -> msg)
        )

      case ServerMessage(msg) =>
        send(
          ("t" -> "sm") ~
            ("msg" -> msg)
        )

      case WebSocketPing | WebSocketPong =>
    }
  }

  override def postStop(): Unit = {
    log.debug("postStop")
    player.foreach {_ ! Player.Disconnected}
    super.postStop()
  }
}

object Client {

  case class ServerMessage(msg:String)

  case class PlayerAdded(uuid: String, p: Point)

  case class TilesAdded(chunk: Long, tiles: List[TileStub])

  case class TileStub(tile:String, standable:Boolean, overlay:Option[String], p:Point)

  case class TilesRemoved(chunks: List[Long])

  case class PlayerRemoved(uuid: String)

  case class PlayerBound(player: ActorRef)

  case class Received(json: JValue)

  case class PlayerMoved(uuid: String, ts: Long, p: Point)

  case class PlayerTalking(uuid: String, msg:String)

}