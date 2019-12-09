package jc.minesweeper

import scala.collection.immutable
import scala.util.Random

sealed trait Cell {
  def expose: String
  def markedString: String = " X "
  def unexposed: String = " # "
}
case class Bomb(marked: Boolean = false) extends Cell {
  override def toString: String = if (marked) markedString else unexposed
  override def expose: String = " * "
}
case class Empty(nearBy: Int, exposed: Boolean = false, marked: Boolean = false)
    extends Cell {
  override def toString: String =
    if (marked) markedString else if (exposed) expose else unexposed

  override def expose: String = if (nearBy > 0) f" $nearBy%d " else " _ "
}

object Cells {
  type Cells = List[List[Cell]]

  def apply(size: Int, bombs: Int): Cells = {
    @scala.annotation.tailrec
    def loop(cells: Cells, bombs: List[(Int, Int)]): Cells = {
      bombs match {
        case Nil => cells
        case (x, y) :: t =>
          loop(placeBomb(cells, x, y), t)
      }
    }

    val cells = List.fill(size)(List.fill(size)(Empty(0)))
    val bombList =
      (1 to bombs)
        .map(_ => (Random.nextInt(size), Random.nextInt(size)))
        .distinct
        .toList

    loop(cells, bombList)
  }

  def placeBomb(cells: Cells, x: Int, y: Int): Cells = {
    @scala.annotation.tailrec
    def loop(cells: Cells, around: List[(Int, Int)]): Cells = {
      around match {
        case Nil => cells
        case (x, y) :: t =>
          getCell(cells, x, y) match {
            case Some(Bomb(_)) => loop(cells, t)
            case Some(e @ Empty(nearBy, _, _)) =>
              loop(replaceCell(cells, x, y, e.copy(nearBy = nearBy + 1)), t)
          }
      }
    }
    getCell(cells, x, y) match {
      case Some(_: Empty) =>
        val checks: Seq[(Int, Int)] = surrounding(cells, x, y)
        loop(replaceCell(cells, x, y, Bomb()), checks.toList)
      case _ => cells
    }
  }

  def won(cells: Cells): Boolean = {
    cells.foldRight(true) {
      case (row, ok) =>
        row.foldRight(ok) {
          case (cell, ok) =>
            ok && (cell match {
              case Empty(_, exposed, _) => exposed
              case _                    => true
            })
        }
    }
  }

  def exposeCell(cells: Cells, x: Int, y: Int): Option[Cells] = {
    @scala.annotation.tailrec
    def loop(cells: Cells, checks: List[(Int, Int)]): Cells = {
      checks match {
        case Nil => cells
        case (x, y) :: t =>
          getCell(cells, x, y) match {
            case Some(e @ Empty(0, false, false)) =>
              val around = (surrounding(cells, x, y) ++ t).distinct.toList
              loop(replaceCell(cells, x, y, e.copy(exposed = true)), around)
            case Some(e @ Empty(_, false, false)) =>
              loop(replaceCell(cells, x, y, e.copy(exposed = true)), t)
            case _ => loop(cells, t)
          }
      }
    }

    getCell(cells, x, y) match {
      case Some(Bomb(false)) => None
      case Some(e @ Empty(0, false, false)) =>
        val checks = surrounding(cells, x, y)
        Some(
          loop(replaceCell(cells, x, y, e.copy(exposed = true)), checks.toList))
      case Some(e @ Empty(_, false, false)) =>
        Some(replaceCell(cells, x, y, e.copy(exposed = true)))
      case _ => Some(cells)
    }
  }

  private def getCell(cells: Cells, x: Int, y: Int): Option[Cell] = {
    for {
      row <- cells.lift(y)
      cell <- row.lift(x)
    } yield cell
  }

  def markCell(cells: Cells, x: Int, y: Int): Cells = {
    getCell(cells, x, y) match {
      case Some(Bomb(m)) => replaceCell(cells, x, y, Bomb(marked = !m))
      case Some(e @ Empty(_, _, m)) =>
        replaceCell(cells, x, y, e.copy(marked = !m))
      case _ => cells
    }
  }

  private def replaceCell(cells: Cells,
                          x: Int,
                          y: Int,
                          cell: => Cell): Cells = {
    cells.updated(y, cells(y).updated(x, cell))
  }

  private def surrounding(cells: Cells, x: Int, y: Int): Seq[(Int, Int)] = {
    if (!cells.indices.contains(y) || !cells(y).indices.contains(x)) {
      Seq()
    } else {
      val ys: immutable.Seq[Int] = y match {
        case ay if ay == 0                => ay to ay + 1
        case ay if ay == cells.length - 1 => ay - 1 to ay
        case ay                           => ay - 1 to ay + 1
      }
      val xs: immutable.Seq[Int] = x match {
        case ax if ax == 0                   => ax to ax + 1
        case ax if ax == cells(y).length - 1 => ax - 1 to ax
        case ax                              => ax - 1 to ax + 1
      }
      for {
        ax <- xs
        ay <- ys
        if !(ax == x && ay == y)
      } yield (ax, ay)
    }
  }
}
