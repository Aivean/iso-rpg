package com.aivean.isorpg.game.chars

import akka.actor.{ActorLogging, Actor, Props}
import com.aivean.isorpg.game.{World, Point}
import com.aivean.isorpg.game.Tiles.TileType

import scala.concurrent.duration._
import scala.language.postfixOps
import scala.util.Random

/**
  *
  * @author <a href="mailto:ivan.zaytsev@webamg.com">Ivan Zaytsev</a>
  *         2015-11-29
  */
class Monster private (override val initialPos:Point) extends Actor with ActorLogging with MovingObject {
  import Monster._

  var lastArrivedAt = System.currentTimeMillis()
  def longTimeNoMovement = System.currentTimeMillis() - lastArrivedAt > (3 second).toMillis

  implicit val ec = context.dispatcher

  self ! UpdateMovement

  def receive = movingObjectBehavior.orElse({
    case UpdateMovement =>
      if (movingTarget.isEmpty || longTimeNoMovement) {
        context.parent ! World.SurroundingsRequest(pos, 5)
      }
      context.system.scheduler.scheduleOnce(
        (1.7 + Random.nextDouble() * 2) seconds, self, UpdateMovement)

    case SurroundingsView(tiles) =>
      Random.shuffle(tiles.toList.collect {
        case (p, t) if t.standable && !tiles.get(p.up).exists(!_.passable) => p.up
      }).headOption.foreach {
        p => setMovementTarget(p)
      }
  })

  override def onArrivedAt(): Unit = {
    lastArrivedAt = System.currentTimeMillis()
  }
}

object Monster {
  def props(pos:Point) = Props(new Monster(pos))

  private case object UpdateMovement

  case class SurroundingsView(tiles:Map[Point, TileType])
}