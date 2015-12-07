package com.aivean.isorpg.game.chars

import akka.actor.{Actor, ActorLogging, Cancellable}
import akka.event.LoggingReceive
import com.aivean.isorpg.game.{Point, World}

import scala.concurrent.duration.DurationDouble
import scala.language.postfixOps

/**
  *
  * @author <a href="mailto:ivan.zaytsev@webamg.com">Ivan Zaytsev</a>
  *         2015-11-29
  */
trait StatePolling {
  selfActor: Actor with ActorLogging =>

  import StatePolling._

  def initialPos: Point

  def movingTarget = _stateTarget

  private var _stateTarget: Option[State] = None
  private var stateUpdate: Option[Cancellable] = None

  def pos = _pos

  private var _pos = initialPos

  def setTarget(t: State): Unit = {
    _stateTarget = Some(t)
    log.debug(s"set target $t")
    self ! UpdateState
  }

  def onArrivedAt() = {}

  private def checkStateCompletion(msg: Any) = _stateTarget.foreach { t =>
    stateUpdate.foreach(_.cancel())
    stateUpdate = None
    t.complete.andThen{_ => _stateTarget = None}.orElse[Any, Unit] { case _ => self ! UpdateState }.apply(msg)
  }

  def movingObjectBehavior: Receive = LoggingReceive {

    case UpdateState =>
      implicit val ec = context.dispatcher

      _stateTarget.foreach { target =>
        log.debug(s"updating target $target")

        target.poll

        stateUpdate = stateUpdate.orElse(
          Some(context.system.scheduler.schedule(0.5 seconds, 0.5 seconds, self, UpdateState)))
      }

    case msg@ArrivedAt(p) =>
      _pos = p
      onArrivedAt()
      checkStateCompletion(msg)
  }


  sealed trait State {
    def complete: Receive

    def poll(): Unit
  }

  sealed case class MovingToState(p: Point) extends State {
    override def complete = {
      case ArrivedAt(p) if p == this.p =>
    }

    override def poll():Unit = {
      context.parent ! World.PlayerWantsToMove(p)
    }
  }

  sealed case class AttackState(targetUuid:String) extends State {
    override def complete: Receive = {
      case AttackTargetDoesNotExist(uuid) if uuid == targetUuid =>
    }

    override def poll(): Unit = {
      context.parent ! World.PlayerWantsToAttack(targetUuid)
    }
  }
}

object StatePolling {

  //messages

  case class ArrivedAt(p: Point)

  case class AttackTargetDoesNotExist(uuid:String)

  case object UpdateState
}
