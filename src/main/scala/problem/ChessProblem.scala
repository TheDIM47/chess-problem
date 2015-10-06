package problem

import scala.annotation.tailrec
import scala.collection.parallel.ParSeq

object Chess {

  def printBoard(rows:Int, columns:Int, board: Array[Byte]): String = {
    val hdr = "--" * columns + "-" * (columns + 1) + "\n"
    def mks[T](xs: Array[T]): String = {
      if (xs.length == 0) ""
      else {
        val (l, r) = xs.splitAt(columns)
        "|" + l.map(x => if (x == 1) "  " else x.formatted("%2d")).mkString("|") + "|\n" + hdr + mks(r)
      }
    }
    hdr + mks(board)
  }

  def solver(rows: Int, columns: Int, figures: List[ChessFigure]): Seq[Array[Byte]] = {

    def isValid(b: Array[Byte]): Boolean = {
      val board = Board(rows, columns, b)

      @tailrec
      def isValidR(brd: Board, index: Int): Boolean = {
        if (index == brd.board.size) true
        else if (brd.isEmpty(index) || brd.board(index) < King.weight) isValidR(brd, index + 1)
        else {
          val (r, c) = brd.i2rc(index)
          val weight = brd.board(index)
          require(weight >= 10 && weight <= 50)
          val f = if (weight == 50) Queen
          else if (weight == 40) Rook
          else if (weight == 30) Bishop
          else if (weight == 20) Knight
          else King
          brd.trySet(r, c, f) && isValidR(brd, index + 1)
        }
      }

      val r = isValidR(board, 0)
      r
    }

    val board = new Array[Byte](rows * columns)
    for (i <- 0 until figures.size) {
      board(i) = figures(i).weight
    }

    board.permutations.filter(x => isValid(x)).toSeq
  }

  @tailrec
  def shift[T](xs: List[T], n: Int = 1): List[T] = {
    if (n == 0) xs
    else shift(xs.tail ::: xs.head :: Nil, n - 1)
  }

  def remove[T](a: T, xs: List[T]): List[T] = {
    remove(xs.indexOf(a), xs)
  }

  def remove[T](i: Int, xs: List[T]): List[T] = {
    val (left, right) = xs.splitAt(i)
    left ::: right.drop(1)
  }

  trait IBoard {
    val rows: Int
    val columns: Int
  }

  // 0 - empty
  // 1 - busy (attacked)
  // X - some figure

  // Board

  type Point = (Int, Int)

  sealed trait Figure {
    val weight: Byte
    val name: String
  }

  sealed trait Move {
    def moves[T](board: IBoard, r: Int, c: Int): Seq[Point]
  }

  sealed trait ChessFigure extends Figure with Move

  // Король
  case object King extends ChessFigure {
    val weight: Byte = 10
    val name: String = "K"

    def moves[T](board: IBoard, r: Int, c: Int): Seq[Point] = {
      for {
        row <- math.max(0, r - 1) to math.min(board.rows - 1, r + 1)
        col <- math.max(0, c - 1) to math.min(board.columns - 1, c + 1)
        if ((row, col) !=(r, c))
      } yield (row, col)
    }
  }

  // Конь
  case object Knight extends ChessFigure {
    val weight: Byte = 20
    val name: String = "N"

    def moves[T](board: IBoard, r: Int, c: Int): Seq[Point] = {
      val m1 = for {
        col <- Seq(c - 2, c + 2)
        row <- Seq(r - 1, r + 1)
        if (col >= 0 && col < board.columns && row >= 0 && row < board.rows)
      } yield (row, col)
      val m2 = for {
        col <- Seq(c - 1, c + 1)
        row <- Seq(r - 2, r + 2)
        if (col >= 0 && col < board.columns && row >= 0 && row < board.rows)
      } yield (row, col)
      m1 ++ m2
    }
  }

  // Слон
  case object Bishop extends ChessFigure {
    val weight: Byte = 30
    val name: String = "B"

    def moves[T](board: IBoard, r: Int, c: Int): Seq[Point] = {
      // ok - left-bottom -- top-right
      val b1 = r - c
      val start1 = if (b1 < 0) -b1 else 0
      val stop1 = math.min(board.columns, board.rows - b1)
      val m1 = for {
        x <- start1 until stop1
        if ((x + b1, x) !=(r, c))
      } yield (x + b1, x)

      // ok top-left -- bottom-right
      val b2 = r + c
      val start2 = if (b2 < board.rows) 0 else 1 + (b2 - board.rows)
      val stop2 = math.min(board.columns, b2 + 1)
      val m2 = for {
        x <- start2 until stop2
        if ((-x + b2, x) !=(r, c))
      } yield (-x + b2, x)
      //
      m1 ++ m2
    }
  }

  // Ладья
  case object Rook extends ChessFigure {
    val weight: Byte = 40
    val name: String = "R"

    def moves[T](board: IBoard, r: Int, c: Int): Seq[Point] = {
      val m1 = for (row <- 0 until board.rows; if row != r) yield (row, c)
      val m2 = for (col <- 0 until board.columns; if col != c) yield (r, col)
      m1 ++ m2
    }
  }

  // Ферзь
  case object Queen extends ChessFigure {
    val weight: Byte = 50
    val name: String = "Q"

    private val rook = Rook
    private val bishop = Bishop

    def moves[T](board: IBoard, r: Int, c: Int): Seq[Point] = {
      rook.moves(board, r, c) ++ bishop.moves(board, r, c)
    }
  }

  case class Board(rows: Int, columns: Int, board: Array[Byte]) extends IBoard {
    require(rows > 0 && columns > 0 && board.size == (rows * columns))
    def this(rows: Int, columns: Int) = this(rows, columns, new Array[Byte](rows * columns))

    def i2rc(idx: Int): Point = (idx / columns, idx % columns)

    private def rc2i(r: Int, c: Int): Int = {
      require(r >= 0 && r < rows && c >= 0 && c < columns)
      val index = r * columns + c
      index
    }

    def isEmpty(r: Int, c: Int): Boolean = isEmpty(rc2i(r, c))

    def isEmpty(index: Int): Boolean = board(index) == 0

    def isSafe(p: Point): Boolean = board(rc2i(p._1, p._2)) < 2

    @tailrec
    final def isSafe(moves: Seq[Point]): Boolean = {
      if (moves.isEmpty) true else isSafe(moves.head) && isSafe(moves.tail)
    }

    def busy(r: Int, c: Int): Unit = board(rc2i(r, c)) = 1

    def trySet(r: Int, c: Int, figure: Figure with Move): Boolean = {
      //      if (isEmpty(r, c)) {
      val key = (this.rows, this.columns, figure.weight, (r, c))
      val m = moveMap.getOrElse(key, {
        val m = figure.moves(this, r, c) // get valid moves
        moveMap += key -> m
        m
      })
      val result = isSafe(m) // check safety
      //        if (result) {
      //          board(rc2i(r, c)) = figure.weight // set figure to (r,c)
      //          m.filter(p => isEmpty(p._1, p._2)).foreach(p => busy(p._1, p._2)) // set fields to "busy" state
      //        }
      result
      //      } else false
    }

  }

  var moveMap = Map[(Int, Int, Byte, Point), Seq[Point]]()
}
