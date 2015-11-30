package com.aivean.isorpg.game.chars

import akka.actor.{Props, Actor, ActorRef}
import com.aivean.isorpg.game.Tiles.TileType
import com.aivean.isorpg.game.{Point, World}
import com.aivean.isorpg.routes.Client

/**
  *
  * @author <a href="mailto:ivan.zaytsev@webamg.com">Ivan Zaytsev</a>
  *         2015-11-29
  */
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