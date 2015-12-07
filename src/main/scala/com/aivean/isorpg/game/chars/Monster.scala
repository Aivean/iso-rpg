package com.aivean.isorpg.game.chars

import akka.actor.{Actor, ActorLogging, Props}
import com.aivean.isorpg.game.Tiles.TileType
import com.aivean.isorpg.game.{Point, World}

import scala.collection.immutable.Queue
import scala.concurrent.duration._
import scala.language.postfixOps
import scala.util.Random

/**
  *
  * @author <a href="mailto:ivan.zaytsev@webamg.com">Ivan Zaytsev</a>
  *         2015-11-29
  */
class Monster private (override val initialPos:Point) extends Actor with ActorLogging with StatePolling {
  import Monster._

  var lastArrivedAt = System.currentTimeMillis()
  var lastQ = Queue[(Point, Long)](initialPos -> System.currentTimeMillis())
  def longTimeNoMovement = System.currentTimeMillis() - lastArrivedAt > (3 second).toMillis

  def cyclicMovement = {
    val (lastP, lastTs) = lastQ.last
    lastQ.tail.collect{case (`lastP`, ts) if lastTs - ts < (3 seconds).toMillis => true}.nonEmpty
  }

  implicit val ec = context.dispatcher

  self ! UpdateMovement

  def receive = movingObjectBehavior.orElse({
    case UpdateMovement =>
      if (movingTarget.isEmpty || longTimeNoMovement || cyclicMovement) {
        context.parent ! World.SurroundingsRequest(pos, 5)
      }
      context.system.scheduler.scheduleOnce(
        (1.7 + Random.nextDouble() * 2) seconds, self, UpdateMovement)

    case SurroundingsView(tiles) =>
      Random.shuffle(tiles.toList.collect {
        case (p, t) if t.standable && !tiles.get(p.up).exists(!_.passable) => p.up
      }).headOption.foreach {
        p => setTarget(MovingToState(p))
      }
  })

  override def onArrivedAt(): Unit = {
    lastArrivedAt = System.currentTimeMillis()
    lastQ = lastQ.enqueue(pos -> lastArrivedAt)
    while (lastQ.size > 4) {lastQ = lastQ.tail}
  }
}

object Monster {
  def props(pos:Point) = Props(new Monster(pos))

  private case object UpdateMovement

  case class SurroundingsView(tiles:Map[Point, TileType])
}