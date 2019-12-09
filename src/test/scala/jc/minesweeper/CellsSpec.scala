package jc.minesweeper

import jc.minesweeper.Cells.Cells
import org.scalatest._

class CellsSpec extends FlatSpec with Matchers {
  val emptyGrid: Cells = Cells(4, 0)
  val one: Empty = Empty(1)

  "Cells" should "create an empty grid" in {
    Cells(4, 0) shouldEqual emptyGrid
  }

  "Cells" should "increment surrounding empty cells when adding a bomb" in {
    val cells = Cells.placeBomb(emptyGrid, 1, 1)
    cells(1)(1) shouldEqual Bomb()

    val checks = for {
      x <- 0 to 2
      y <- 0 to 2
      if !(x == 1 && y == 1)
    } yield (x, y)
    val allChecks =
      checkCells(cells, checks.toList, _ equals one)
    allChecks shouldEqual true
  }

  private def checkCells(cells: Cells,
                         checks: List[(Int, Int)],
                         check: Cell => Boolean): Boolean = {
    val bools = for {
      y <- cells.indices
      x <- cells(y).indices
      cell = cells(y)(x)
    } yield if (checks.contains((x, y))) check(cell) else !check(cell)
    bools.forall(identity)
  }

  "Cells" should "increment empty surrounding cells on the side and corner" in {
    val cells1 = Cells.placeBomb(emptyGrid, 0, 0)
    val cells1Matchers = List((0, 1), (1, 0), (1, 1))
    val allChecks1 = checkCells(cells1, cells1Matchers, _ equals one)
    allChecks1 shouldEqual true

    val cells2 = Cells.placeBomb(emptyGrid, 1, 0)
    val cells2Matchers = List((0, 0), (2, 0), (0, 1), (1, 1), (2, 1))
    val allChecks2 = checkCells(cells2, cells2Matchers, _ equals one)
    allChecks2 shouldEqual true
  }

  "Cells" should "expose all cells when there's no bombs" in {
    val maybeChecked = for {
      cells <- Cells.exposeCell(emptyGrid, 0, 0)
      checked <- Some(checkCells(cells, List(), {
        case Bomb(_)           => true
        case Empty(_, true, _) => false
        case _                 => true
      }))
    } yield checked
    maybeChecked shouldEqual Some(true)
  }

  "Cells" should "expose up to the cells with nearby bombs" in {
    val cells = Cells.placeBomb(Cells.placeBomb(emptyGrid, 0, 1), 1, 0)
    val shouldBeExposed = List((3, 3),
                               (3, 2),
                               (3, 1),
                               (3, 0),
                               (2, 3),
                               (1, 3),
                               (0, 3),
                               (2, 2),
                               (2, 1),
                               (2, 0),
                               (1, 2),
                               (0, 2),
                               (1, 1))
    checkCells(cells, shouldBeExposed, {
      case Empty(_, true, _) => true
      case _                 => false
    })
  }
}
