package com.aivean.isorpg.game

import scala.util.Random

/**
  *
  * @author <a href="mailto:ivan.zaytsev@webamg.com">Ivan Zaytsev</a>
  *         2015-11-15
  */
object Tiles {

  sealed trait TileType {
    def tile: String
    def standable: Boolean
    def passable: Boolean
    def overlay:Option[String] = None
  }

  case class Water(tile:String = "water-open-0000-01") extends TileType {
    //    def tile = "water-open-0000-%02d".format(variant)
    def standable = false
    def passable = true
  }

  object Water {
    /**
      * S SW W NW N NE E SE
      * @param neighbours true if NOT WATER, false - water
      */
    def apply(neighbours: Seq[Boolean]):Water = neighbours.map(c => if (c) 1 else 0) match {
      case Seq(1, _, 1, _, 1, _, 1, _) => Water("water-pool")

      case Seq(1, _, 1, _, 1, _, 0, _) => Water("water-closed-1110")
      case Seq(1, _, 1, _, 0, _, 1, _) => Water("water-closed-1101")
      case Seq(1, _, 0, _, 1, _, 1, _) => Water("water-closed-1011")
      case Seq(0, _, 1, _, 1, _, 1, _) => Water("water-closed-0111")

      case Seq(1, _, 0, 1, 0, _, 1, _) => Water("water-closed-1001")
      case Seq(1, _, 0, _, 1, _, 0, _) => Water("water-closed-1010")
      case Seq(0, _, 1, _, 0, _, 1, _) => Water("water-closed-0101")
      case Seq(1, _, 1, _, 0, 1, 0, _) => Water("water-closed-1100")
      case Seq(0, 1, 0, _, 1, _, 1, _) => Water("water-closed-0011")
      case Seq(0, _, 1, _, 1, _, 0, 1) => Water("water-closed-0110")

      case Seq(0, _, 0, _, 0, _, 1, _) => Water("water-open-0001")
      case Seq(0, _, 0, _, 1, _, 0, _) => Water("water-open-0010")
      case Seq(0, _, 0, _, 1, _, 1, _) => Water("water-open-0011")
      case Seq(0, _, 1, _, 0, _, 0, _) => Water("water-open-0100")
      case Seq(0, _, 1, _, 1, _, 0, _) => Water("water-open-0110")
      case Seq(1, _, 0, _, 0, _, 0, _) => Water("water-open-1000")
      case Seq(1, _, 0, _, 0, _, 1, _) => Water("water-open-1001")
      case Seq(1, _, 1, _, 0, _, 0, _) => Water("water-open-1100")

      case Seq(0, 1, 0, _, 0, _, 0, _) => Water("water-corner-l")
      case Seq(0, _, 0, 1, 0, _, 0, _) => Water("water-corner-u")
      case Seq(0, _, 0, _, 0, 1, 0, _) => Water("water-corner-r")
      case Seq(0, _, 0, _, 0, _, 0, 1) => Water("water-corner-d")

      case _ if Random.nextInt(100) < 5 => Water("water-open-0000-03")
      case _ if Random.nextInt(100) < 5 => Water("water-open-0000-04")
      case _ if Random.nextInt(2) == 0 => Water("water-open-0000-01")
      case _ => Water("water-open-0000-02")
    }
  }

  case class Stone(passable:Boolean) extends TileType {
    val standable = false

    val tile = if (passable) Seq(
        "stone01","stone02","stone03"
      )(Random.nextInt(3))
    else Seq(
      "stone04","stone05"
    )(Random.nextInt(2))
  }

  case class Log() extends TileType {
    val tile = "log0" + (Random.nextInt(2) + 1)
    override def standable: Boolean = false
    override def passable: Boolean = false
  }

  case class HighGrass() extends TileType {
    val tile = "higrass-1111"
    override def standable: Boolean = false
    override def passable: Boolean = true
  }

  case class Bush() extends TileType {
    val tile = Seq(
        "bush-01-large",
        "bush-01-med",
        "bush-01-small",
        "bush-02-small",
        "bush-03-large",
        "bush-03-small",
        "bush-dead"
    ) match {
      case x => x(Random.nextInt(x.size))
    }

    val standable = false
    val passable = false
  }

  case class Tree() extends TileType {
    val tile = Seq(
        "tree-large-dead",
        "tree-large",
        "tree-med-dead",
        "tree-med",
        "tree-pine-large",
        "tree-pine-med"
    ) match {
      case x => x(Random.nextInt(x.size))
    }

    val standable = false
    val passable = false
  }

  case class GrassSlope(south: Int, west: Int, north: Int, east: Int) extends TileType {
    require(north >= 0 && north <= 2, "up must be in range 0..2")
    require(south >= 0 && south <= 2, "down must be in range 0..2")
    require(west >= 0 && south <= 2, "left must be in range 0..2")
    require(east >= 0 && east <= 2, "right must be in range 0..2")

    def standable = true
    def passable = false
    def tile = "grass-slope-%04d".format(south * 1000 + west * 100 + north * 10 + east)

    override val overlay = if (south != 2 || west != 2 || north != 2 || east != 2) None
    else if (Random.nextBoolean()) Some("grass%02d".format(Random.nextInt(18) + 1)) else None
  }

  case object GrassSlope {
    private val slopes = Seq(
      // s w n e
      Seq(0, 0, 1, 1),
      Seq(0, 1, 1, 0),
      Seq(0, 1, 2, 1),
      Seq(1, 0, 0, 1),
      Seq(1, 0, 1, 2),
      Seq(1, 1, 0, 0),
      Seq(1, 1, 2, 2),
      Seq(1, 2, 1, 0),
      Seq(1, 2, 2, 1),
      Seq(2, 1, 0, 1),
      Seq(2, 1, 1, 2),
      Seq(2, 2, 1, 1),
      Seq(2, 2, 2, 2))

    /**
      * @param s Seq(S, W, N, E) -> rating in range 0 to 2
      * @return
      */
    def find(s: Seq[Int]) = {
      def f(ref: Int, act: Int) =
        if (ref == act) 0
        else if (act == 1 && ref == 2) 1
        else if (act == 0 && ref == 2) 2
        else if (act == 0 && ref == 1) 2
        else 3

      (s match {
        case Seq(0, 0, 0, 0) => Seq(2, 2, 2, 2)
        case x if slopes.contains(x) => x
        case _ if s.count(_ == 2) >= 1 && !s.contains(0) => Seq(2, 2, 2, 2)
        case Seq(ss, ww, nn, ee) =>
          val s1 = List(ss, ww, nn, ee)
          slopes.map(s => (s, s.zip(s1).map(x => math.abs(x._1 - x._2)).sum))
            .sortBy(_._2).head._1
      }) match {
        case Seq(ss, ww, nn, ee) => GrassSlope(ss, ww, nn, ee)
      }
    }
  }

}
