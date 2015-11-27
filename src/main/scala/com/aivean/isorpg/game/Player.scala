package com.aivean.isorpg.game

import akka.actor.{ActorLogging, Actor, Cancellable, Props}
import akka.event.LoggingReceive

import scala.concurrent.duration.DurationDouble
import scala.language.postfixOps

/**
  *
  * @author <a href="mailto:ivan.zaytsev@webamg.com">Ivan Zaytsev</a>
  *         2015-11-13
  */

class Player private extends Actor with ActorLogging {

  import Player._

  var movingTarget: Option[Point] = None
  var movementUpdate: Option[Cancellable] = None
  implicit val ec = context.dispatcher

  def receive = LoggingReceive {
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

  case class RequestMoveTo(p: Point)

  case class Say(msg: String)

  case class ArrivedAt(p: Point)

  case object UpdateMovement

  case object Disconnected

  def props = Props(new Player)
}
