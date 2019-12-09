package jc.minesweeper

import cats.effect._
import com.github.battermann.pureapp._
import com.github.battermann.pureapp.interpreters.Terminal
import jc.minesweeper.Cells.Cells

import scala.util.Try

object Minesweeper extends SimplePureApp[IO] {

  sealed trait State

  case object Unstarted extends State

  case object Playing extends State

  case object Done extends State

  // MODEL
  final case class Model(state: State = Unstarted,
                         cells: Option[Cells] = None,
                         outputMsg: String = " Shall we play a game? ")

  sealed trait Msg

  case object Quit extends Msg

  case object InvalidInput extends Msg

  final case class NewGame(size: Int = 9, bombs: Int = 9) extends Msg

  final case class Expose(x: Int, y: Int) extends Msg

  final case class Mark(x: Int, y: Int) extends Msg

  def init: Model = Model()

  def quit(msg: Msg): Boolean = msg == Quit

  // UPDATE
  def update(msg: Msg, model: Model): Model =
    msg match {
      case Quit =>
        model

      case NewGame(size, bombs) =>
        Model(Playing, Some(Cells(size, bombs)), "Choose a cell to expose")

      case Expose(x, y) =>
        model match {
          case Model(Playing, Some(cells), _) =>
            if (y >= cells.length || x >= cells(y).length)
              model.copy(outputMsg = "That's off the board!")
            else {
              val maybeCells = Cells.exposeCell(cells, x, y)
              maybeCells match {
                case Some(newCells) =>
                  if (Cells.won(newCells))
                    Model(Done, Some(newCells), "You win!")
                  else Model(Playing, Some(newCells), s"Exposed cell at $x, $y")
                case None =>
                  Model(Done,
                        Some(cells),
                        s"~~~~~~~~BOOOOMMMMM~~~~~~ Hit a bomb at $x, $y")
              }
            }
          case m => m.copy(outputMsg = "Need to start a new game!")
        }

      case Mark(x, y) =>
        model match {
          case Model(Playing, Some(cells), _) =>
            if (y >= cells.length || x >= cells(y).length)
              model.copy(outputMsg = "That's off the board!")
            else {
              Model(Playing,
                    Some(Cells.markCell(cells, x, y)),
                    s"Marked cell at $x, $y")
            }
          case m => m.copy(outputMsg = "Need to start a new game!")
        }

      case InvalidInput =>
        model.copy(outputMsg = "Unrecognised command or option")
    }

  def parse(input: String): Msg = {
    if (input == "q") Quit
    else {
      Try {
        val strings = input.split(" ").toList
        val cmd = strings.head
        val params = strings.tail.map(_.toInt)

        cmd match {
          case "n" =>
            params match {
              case Seq(size, bombs) => NewGame(size, bombs)
              case Seq(size)        => NewGame(size)
              case Nil              => NewGame()
              case _                => InvalidInput
            }
          case "e" =>
            params match {
              case Seq(x, y) => Expose(x, y)
              case _         => InvalidInput
            }
          case "m" =>
            params match {
              case Seq(x, y) => Mark(x, y)
              case _         => InvalidInput
            }
        }
      }.getOrElse(InvalidInput)
    }
  }

  def printUsage: IO[Unit] = Terminal.putStrLn(
    """
      |  Command   Arguments            Purpose
      |
      |  n         <size> <bombs>       Start a new game (defaults to 9, 9)
      |  e         <x> <y>              Expose the cell at x,y
      |  m         <x> <y>              Mark the cell at x,y
      |  q                              Exit the app
      |""".stripMargin
  )

  def getAndParseInput: IO[Msg] =
    Terminal.putStr(">>> ").flatMap(_ => Terminal.readLine.map(parse))

  def printModel(model: Model): String = {
    def showBoard(cells: Cells, outputMsg: String, mapCell: Cell => String) = {
      val lines: List[String] = cells.zipWithIndex.map({
        case (row: List[Cell], index: Int) =>
          f"$index%2d " + row.map(mapCell).mkString("")
      })
      val header = "   " + cells.head.indices
        .map(i => f" $i%d ")
        .mkString("") + "\n"
      header + lines.mkString("\n") + s"\n$outputMsg\n"
    }

    model match {
      case Model(Unstarted, _, outputMsg) =>
        s"Minesweeper: No game started\n$outputMsg\n"
      case Model(Playing, Some(cells), outputMsg) =>
        showBoard(cells, outputMsg, _.toString)
      case Model(Done, Some(cells), outputMsg) =>
        showBoard(cells, outputMsg, _.expose)
      case Model(_, None, outputMsg) => outputMsg
    }
  }

  // IO

  def io(model: Model): IO[Msg] =
    for {
      _ <- Terminal.putStrLn(printModel(model))
      _ <- printUsage
      msg <- getAndParseInput
    } yield msg
}
