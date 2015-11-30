package com.aivean.isorpg.game.chars

import akka.actor.{Actor, ActorLogging, Cancellable}
import com.aivean.isorpg.game.{Point, World}

import scala.concurrent.duration.DurationDouble
import scala.language.postfixOps

/**
  *
  * @author <a href="mailto:ivan.zaytsev@webamg.com">Ivan Zaytsev</a>
  *         2015-11-29
  */
trait MovingObject {
  selfActor: Actor with ActorLogging =>

  import MovingObject._

  def initialPos: Point

  def movingTarget = _movingTarget

  private var _movingTarget: Option[Point] = None
  private var movementUpdate: Option[Cancellable] = None

  def pos = _pos

  private var _pos = initialPos

  def setMovementTarget(p: Point): Unit = {
    _movingTarget = Some(p)
    log.debug(s"set target $p")
    self ! UpdateMovement
  }

  def onArrivedAt() = {}

  def movingObjectBehavior: Receive = {

    case UpdateMovement =>
      implicit val ec = context.dispatcher

      _movingTarget.foreach { moveTo =>
        log.debug(s"updating movement $moveTo")
        context.parent ! World.PlayerWantsToMove(moveTo)
        movementUpdate = movementUpdate.orElse(
          Some(context.system.scheduler.schedule(0.5 seconds, 0.5 seconds, self, UpdateMovement)))
      }

    case ArrivedAt(p) =>
      _pos = p
      onArrivedAt()
      movementUpdate.foreach(_.cancel())
      movementUpdate = None
      if (_movingTarget.contains(p)) {
        _movingTarget = None
      } else {
        self ! UpdateMovement
      }
  }
}

object MovingObject {
  case class ArrivedAt(p: Point)

  case object UpdateMovement
}
