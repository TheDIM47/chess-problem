import org.scalatest._
import problem.Chess.{Bishop, Board}

class BishopMoveSpec extends WordSpec with ShouldMatchers {

  /** *
  trait Bishop extends Move {
    def moves[T](board: Board, r: Int, c: Int): Seq[Point] = {
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
    * **/

  "Bishop Move" when {

    "1x1" should {
      val board: Board = new Board(1, 1)
      "should be empty" in {
        Bishop.moves(board, 0, 0) should be(empty)
      }
    }

    "1x2 for (0,0)" should {
      val board: Board = new Board(1, 2)
      "should be empty" in {
        Bishop.moves(board, 0, 0) should be(empty)
      }
    }

    "1x2 for (0,1)" should {
      val board: Board = new Board(1, 2)
      "should be empty" in {
        Bishop.moves(board, 0, 1) should be(empty)
      }
    }

    "2x1 for (0,0)" should {
      val board: Board = new Board(2, 1)
      "should be empty" in {
        Bishop.moves(board, 0, 0) should be(empty)
      }
    }

    "2x1 for (1,0)" should {
      val board: Board = new Board(2, 1)
      "should be empty" in {
        Bishop.moves(board, 1, 0) should be(empty)
      }
    }

    "1x3 for (0,0)" should {
      val board: Board = new Board(1, 3)
      "should be empty" in {
        Bishop.moves(board, 0, 0) should be(empty)
      }
    }

    "1x3 for (0,1)" should {
      val board: Board = new Board(1, 3)
      "should be empty" in {
        Bishop.moves(board, 0, 1) should be(empty)
      }
    }

    "1x3 for (0,2)" should {
      val board: Board = new Board(1, 3)
      "should be empty" in {
        Bishop.moves(board, 0, 2) should be(empty)
      }
    }


    "3x1 for (0,0)" should {
      val board: Board = new Board(3, 1)
      "should be empty" in {
        Bishop.moves(board, 0, 0) should be(empty)
      }
    }

    "3x1 for (1,0)" should {
      val board: Board = new Board(3, 1)
      "should be empty" in {
        Bishop.moves(board, 1, 0) should be(empty)
      }
    }

    "3x1 for (2,0)" should {
      val board: Board = new Board(3, 1)
      "should be empty" in {
        Bishop.moves(board, 2, 0) should be(empty)
      }
    }

    "3x3 for (0,0)" should {
      val board: Board = new Board(3, 3)
      "have size 2" in {
        Bishop.moves(board, 0, 0).size should be(2)
      }
      "have valid coordinates" in {
        Bishop.moves(board, 0, 0) should contain allOf((1, 1), (2, 2))
      }
    }

    "3x3 for (2,2)" should {
      val board: Board = new Board(3, 3)
      "have size 2" in {
        Bishop.moves(board, 2, 2).size should be(2)
      }
      "have valid coordinates" in {
        Bishop.moves(board, 2, 2) should contain allOf((1, 1), (0, 0))
      }
    }

    "3x3 for (0,2)" should {
      val board: Board = new Board(3, 3)
      "have size 2" in {
        Bishop.moves(board, 0, 2).size should be(2)
      }
      "have valid coordinates" in {
        Bishop.moves(board, 0, 2) should contain allOf((1, 1), (2, 0))
      }
    }

    "3x3 for (2,0)" should {
      val board: Board = new Board(3, 3)
      "have size 2" in {
        Bishop.moves(board, 2, 0).size should be(2)
      }
      "have valid coordinates" in {
        Bishop.moves(board, 2, 0) should contain allOf((1, 1), (0, 2))
      }
    }

    "3x3 for (1,1)" should {
      val board: Board = new Board(3, 3)
      "have size 4" in {
        Bishop.moves(board, 1, 1).size should be(4)
      }
      "have valid coordinates" in {
        Bishop.moves(board, 1, 1) should contain allOf(
          (0, 0), (0, 2),
          (2, 0), (2, 2)
          )
      }
    }

    "3x3 for (2,1)" should {
      val board: Board = new Board(3, 3)
      "have size 2" in {
        Bishop.moves(board, 2, 1).size should be(2)
      }
      "have valid coordinates" in {
        Bishop.moves(board, 2, 1) should contain allOf((1, 0), (1, 2))
      }
    }

    "3x3 for (0,1)" should {
      val board: Board = new Board(3, 3)
      "have size 2" in {
        Bishop.moves(board, 0, 1).size should be(2)
      }
      "have valid coordinates" in {
        Bishop.moves(board, 0, 1) should contain allOf((1, 0), (1, 2))
      }
    }

    "5x5 for (2,3)" should {
      val board: Board = new Board(5, 5)
      "have size 2" in {
        Bishop.moves(board, 2, 3).size should be(6)
      }
      "have valid coordinates" in {
        Bishop.moves(board, 2, 3) should contain allOf(
          (0, 1), (4, 1),
          (1, 2), (3, 2),
          (1, 4), (3, 4)
          )
      }
    }


  }
}
