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

  //  def tempGen:Map[Point, TileType] =
  //    Map (
  //      Point(0,0,0) -> Grass1,
  //      Point(1,0,0) -> Grass1,
  //      Point(2,0,0) -> Grass1,
  //      Point(3,0,0) -> Grass1,
  //      Point(4,0,0) -> Grass1,
  //      Point(4,0,1) -> Grass1,
  //      Point(5,0,0) -> Grass1
  //      //Point(6,0,0) -> Crab
  //    )

  implicit class RichAnyRef(obj: AnyRef) {
    def ??[B](implicit m: ClassTag[B]):Boolean = {
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
    Random.shuffle(res.keys.filter(predicate).toList).take(size + Random.nextInt(size))

  def is(pred: TileType => Boolean)(pointPred: Point)(implicit res: Map[Point, TileType]) =
    res.get(pointPred).exists(pred)


  def apply(size: Int) = {
    implicit var res = Map[Point, TileType]()

    for (x <- 0 to size; y <- 0 to size) {
      res += (Point(x, y, 0) -> GrassSlope(2,2,2,2))
    }


//    /**
//      * Stones
//      */
//    randP(is(_ == Grass))((size * size + Random.nextInt(size * size)) / 150 + 1).flatMap { p =>
//      spill(p, Random.nextInt(20) + 3, is(_ == Grass))
//    }.foreach(p => res += (p -> Stone))
//
//    /**
//      * Windows
//      */
//    randP(is(_ == Stone))(Random.nextInt(size) * size / 150 + 2).foreach { p =>
//      res += (p -> Window)
//    }
//
//    /**
//      * Bushes
//      */
//    randP(is(_ == Grass))((size + Random.nextInt(size)) * size / 50 + 1).foreach { p =>
//      res += (p -> (if (Random.nextBoolean()) Bush1 else Bush2))
//    }
//
//    /**
//      * Mushrooms
//      */
//    randP(is(_ == Grass))(Random.nextInt(size * size) / 150 + 1).foreach { p =>
//      res += (p -> Mushroom)
//    }
//
    /**
      * Lift some grass
      */
    for(i <- 1 to 3) {
      randP(is(_.??[GrassSlope]))((size * size + Random.nextInt(size * size)) / 150 + 1).flatMap { p =>
        spill(p, Random.nextInt(400) + 8, p => is(_.??[GrassSlope])(p) && !res.contains(p.up))
      }.foreach(p => res += (p.up -> GrassSlope(2, 2, 2, 2)))
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
//        }.foreach(p => res += (p.up -> GrassSlope(2, 2, 2, 2)))
//      }
//    }

    /**
      * Remove loosely connected highground
      */
    res.keys.toSet.filter(is(_.??[GrassSlope]))
      .filter(_.adjFlatAll.count(p => res.get(p).exists(_.??[GrassSlope])) < 4)
      .foreach {
      p => res -= p
    }

    /**
      * change grass slopes
      */
    {
      val points = res.keys.toSet.filter(is(_.??[GrassSlope]))

      val rating = points.map(p => (p, p.adjFlatCross.count(res.contains))).toMap.mapValues {
        case 4 => 2
        case _ => 1
      }

      points.filter(p => !res.contains(p.up)).foreach {
        p =>
          res +=
            (p -> GrassSlope.find(Seq(p.south, p.west, p.north, p.east).map(rating.getOrElse(_, 0))))
      }
    }


    /**
      * Water
      */
    {
      val quant = Random.nextInt(size * size / 50) / 4 + size / 4 + 1
      def allowed(p:Point) = p.z ==0 && !res.contains(p.up) && res.get(p).exists(_.??[GrassSlope])

      val water = randP(allowed)(quant).flatMap { p =>
        spill(p, Random.nextInt(size * size / 6) + 1, allowed)
      }.map(_.up).toSet

      water.foreach { p =>
        val neighbors = Seq(p.south, p.south.west, p.west, p.north.west,
          p.north, p.north.east, p.east, p.south.east)
        res += ( p -> Water(neighbors.map(water.contains).map(!_)))
      }
    }


    /**
      * High grass
      */
    {
      val quant = Random.nextInt(size * size / 200) / 4 + size / 4 + 1
      def allowed(p:Point) = !res.contains(p.up) && res.get(p).contains(GrassSlope(2,2,2,2))

      val grass = randP(allowed)(quant).flatMap { p =>
        spill(p, Random.nextInt(size * size / 300) + 1, allowed)
      }.map(_.up).toSet

      grass.foreach { p =>
//        val neighbors = Seq(p.south, p.south.west, p.west, p.north.west,
//          p.north, p.north.east, p.east, p.south.east)
        res += ( p -> HighGrass())
      }
    }






    /**
      * Bushes
      */
    randP(p => !res.contains(p.up) && res.get(p).contains(GrassSlope(2, 2, 2, 2)))(Random.nextInt(size * size / 20 +
      20)).foreach(p => res += (p.up -> Bush()))

    /**
      * Trees
      */
    randP(p => !res.contains(p.up) && res.get(p).contains(GrassSlope(2, 2, 2, 2)))(Random.nextInt(size * size / 30 +
      10)).foreach(p => res += (p.up -> Tree()))

    /**
      * Stones
      */
    randP(p => !res.contains(p.up) && res.get(p).contains(GrassSlope(2, 2, 2, 2)))(Random.nextInt(size * size / 100 +
      10)).foreach(p => res += (p.up -> Stone(Random.nextInt(10) < 8)))

    /**
      * Logs
      */
    randP(p => !res.contains(p.up) && res.get(p).contains(GrassSlope(2, 2, 2, 2)))(Random.nextInt(size * size / 300 +
      10)).foreach(p => res += (p.up -> Log()))

    res
  }


  // def apply(size: Int) = {
  //    var res = Map[Point, TileType]()
  //
  //    for (x <- 0 to size; y <- 0 to size) {
  //      res += (Point(x, y, 0) -> Grass)
  //    }
  //
  //    /**
  //      * Water
  //      */
  //    {
  //      val quant = Random.nextInt(size * size / 50) / 4 + size / 4 + 1
  //      randP(_ => true)(quant).flatMap { p =>
  //        spill(p, Random.nextInt(size * size / 6) + 1, is(_ == Grass))
  //      }.foreach(p => res += (p -> Water))
  //    }
  //
  //    /**
  //      * Stones
  //      */
  //    randP(is(_ == Grass))((size * size + Random.nextInt(size * size)) / 150 + 1).flatMap { p =>
  //      spill(p, Random.nextInt(20) + 3, is(_ == Grass))
  //    }.foreach(p => res += (p -> Stone))
  //
  //    /**
  //      * Windows
  //      */
  //    randP(is(_ == Stone))(Random.nextInt(size) * size / 150 + 2).foreach { p =>
  //      res += (p -> Window)
  //    }
  //
  //    /**
  //      * Bushes
  //      */
  //    randP(is(_ == Grass))((size + Random.nextInt(size)) * size / 50 + 1).foreach { p =>
  //      res += (p -> (if (Random.nextBoolean()) Bush1 else Bush2))
  //    }
  //
  //    /**
  //      * Mushrooms
  //      */
  //    randP(is(_ == Grass))(Random.nextInt(size * size) / 150 + 1).foreach { p =>
  //      res += (p -> Mushroom)
  //    }
  //
  //    /**
  //      * Lift some grass
  //      */
  //    randP(is(_ == Grass))((size * size + Random.nextInt(size * size)) / 150 + 1).flatMap { p =>
  //      spill(p, Random.nextInt(20) + 3, is(_ == Grass))
  //    }.foreach(p => res += (p.up -> Grass1))
  //
  //    res
  //  }
}
