package com.aivean.isorpg.game

import com.aivean.isorpg.game.Tiles._

import scala.collection.mutable
import scala.reflect.ClassTag
import scala.util.Random

/**
  *
  * @author <a href="mailto:ivan.zaytsev@webamg.com">Ivan Zaytsev</a>
  *         2015-11-15
  */
object TerrainGen {

  implicit class RichAnyRef(obj: AnyRef) {
    def ??[B](implicit m: ClassTag[B]): Boolean = {
      obj.getClass.isAssignableFrom(m.runtimeClass)
    }
  }

  def spill(p: Point, size: Int, pred: Point => Boolean, random: Boolean = true) = {
    var result = Set[Point]()
    val q = mutable.PriorityQueue[Point](p)(Ordering.by(-_.distTo(p)))
    var i = size
    while (i > 0 && q.nonEmpty) {
      val p = q.dequeue
      if (pred(p)) {
        i -= 1
        result += p
        (p.adjFlatAll.filterNot(result.contains).filter(pred) match {
          //            case x if random => Random.shuffle(x.toSeq)
          //            case x => x.sortBy(_.distTo(p))
          case x => x
        }).foreach(q.enqueue(_))
      }
    }
    result
  }

  def randP(predicate: Point => Boolean)(size: Int)(implicit res: Map[Point, TileType]) =
    if (size <= 0) Nil
    else Random.shuffle(res.keys.filter(predicate).toList).take(size + Random.nextInt(size))

  def is(pred: TileType => Boolean)(pointPred: Point)(implicit res: Map[Point, TileType]) =
    res.get(pointPred).exists(pred)


  def apply(size: Int) = {
    implicit var res = Map[Point, TileType]()

    for (x <- 0 to size; y <- 0 to size) {
      res += (Point(x, y, 0) -> GrassSlope(3, 3, 3, 3))
    }


    /**
      * Lift some grass
      */
    for (i <- 1 to 3) {
      randP(is(_.??[GrassSlope]))((size * size + Random.nextInt(size * size)) / 150 + 1).flatMap { p =>
        spill(p, Random.nextInt(400) + 8, p => is(_.??[GrassSlope])(p) && !res.contains(p.up))
      }.foreach(p => res += (p.up -> GrassSlope(3, 3, 3, 3)))
    }

    //    /**
    //      * Build high mountain at the center
    //      */
    //    {
    //      val max = size * size / 100
    //      for (i <- 0 until max) {
    //        def isCenter(p: Int) = math.abs(p - (size / 2)) <= (max - i) * size / 4 / max
    //
    //        randP(p => isCenter(p.x) && isCenter(p.y) && is(_.??[GrassSlope])(p))(1).flatMap { p =>
    //          spill(p, Random.nextInt(200) + 8, p => is(_.??[GrassSlope])(p) && !res.contains(p.up))
    //        }.foreach(p => res += (p.up -> GrassSlope(3, 3, 3, 3)))
    //      }
    //    }

    /**
      * Remove lonely highground
      */
    {
      val q = mutable.Queue[Point]()
      val isGrass = is(_.??[GrassSlope]) _

      res.keys.filter(isGrass).foreach(q.enqueue(_))

      def lonely(p:Point) = p.z != 0 &&
        ((p.adjFlatCross :+ p).map(_.down).exists(!isGrass(_)) ||
        !Seq(p.west, p.north, p.east, p.south, p.west).map(isGrass).sliding(2).contains(Seq(true, true)))

      while (q.nonEmpty) {
        val p = q.dequeue()
        if (isGrass(p) && lonely(p)) {
          res -= p
          (p.adjFlatAll :+ p.up).filter(isGrass).foreach(q.enqueue(_))
        }
      }
    }

    /**
      * change grass slopes
      */
    res.foreach {
      case (p, t: GrassSlope) =>
        res += (p -> GrassSlope((p.adjFlatAll :+ p.up).map(is(_.??[GrassSlope]))))
      case _ =>
    }


    /**
      * Water
      */
    {
      val quant = Random.nextInt(size * size / 50 + 1) / 4 + size / 4 + 1
      def allowed(p: Point) = p.z == 0 && !res.contains(p.up) && res.get(p).exists(_.??[GrassSlope])

      val water = randP(allowed)(quant).flatMap { p =>
        spill(p, Random.nextInt(size * size / 6) + 1, allowed)
      }.map(_.up).toSet

      water.foreach { p =>
        res += (p -> Water(p.adjFlatAll.map(water.contains).map(!_)))
      }
    }


    /**
      * High grass
      */
    {
      val quant = Random.nextInt(size * size / 200 + 1) / 4 + size / 4 + 1
      def allowed(p: Point) = !res.contains(p.up) && res.get(p).contains(GrassSlope(3, 3, 3, 3))

      val grass = randP(allowed)(quant).flatMap { p =>
        spill(p, Random.nextInt(size * size / 300 + 1) + 1, allowed)
      }.map(_.up).toSet

      grass.foreach { p =>
        //        val neighbors = Seq(p.south, p.south.west, p.west, p.north.west,
        //          p.north, p.north.east, p.east, p.south.east)
        res += (p -> HighGrass())
      }
    }


    /**
      * Bushes
      */
    randP(p => !res.contains(p.up) && res.get(p).contains(GrassSlope(3, 3, 3, 3)))(Random.nextInt(size * size / 20 +
      size / 15 + 1) + 1).foreach(p => res += (p.up -> Bush()))

    /**
      * Trees
      */
    randP(p => !res.contains(p.up) && res.get(p).contains(GrassSlope(3, 3, 3, 3)))(Random.nextInt(size * size / 30 +
      size / 20 + 1) + 1).foreach(p => res += (p.up -> Tree()))

    /**
      * Stones
      */
    randP(p => !res.contains(p.up) && res.get(p).contains(GrassSlope(3, 3, 3, 3)))(Random.nextInt(size * size / 100 +
      size / 20 + 1)).foreach(p => res += (p.up -> Stone(Random.nextInt(10) < 8)))

    /**
      * Logs
      */
    randP(p => !res.contains(p.up) && res.get(p).contains(GrassSlope(3, 3, 3, 3)))(Random.nextInt(size * size / 300 +
      size / 20 + 1)).foreach(p => res += (p.up -> Log()))

    res
  }
}
