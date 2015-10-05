import org.scalatest._
import problem.Chess.{Knight, Board}

class KnightMoveSpec extends WordSpec with ShouldMatchers {
  "Knight Move" when {
    "1x1" should {
      val board: Board = new Board(1, 1)
      "should be empty" in {
        Knight.moves(board, 0, 0) should be(empty)
      }
    }

    "2x2" should {
      val board: Board = new Board(2, 2)
      "should be empty" in {
        Knight.moves(board, 0, 0) should be(empty)
      }
    }

    "2x3 for (0,0)" should {
      val board: Board = new Board(2, 3)
      "have size 1" in {
        Knight.moves(board, 0, 0).size should be(1)
      }
      "have coordinate (1,2) for (0,0)" in {
        Knight.moves(board, 0, 0) should contain(1, 2)
      }
    }

    "2x3 for (1,2)" should {
      val board: Board = new Board(2, 3)
      "have size 1" in {
        Knight.moves(board, 1, 2).size should be(1)
      }
      "have coordinate (1,2) for (0,0)" in {
        Knight.moves(board, 1, 2) should contain(0, 0)
      }
    }

    "2x3 for (1,1)" should {
      val board: Board = new Board(2, 3)
      "should be empty" in {
        Knight.moves(board, 1, 1) should be(empty)
      }
    }

    "3x3 for (0,0)" should {
      val board: Board = new Board(3, 3)
      "have size 2" in {
        Knight.moves(board, 0, 0).size should be(2)
      }
      "have coordinate (1,2) and (2,1) for (0,0)" in {
        Knight.moves(board, 0, 0) should contain allOf((2, 1), (1, 2))
      }
    }

    "5x5 for (2,2)" should {
      val board: Board = new Board(5, 5)
      "have size 8" in {
        Knight.moves(board, 2, 2).size should be(8)
      }
      "have valid move coordinates" in {
        Knight.moves(board, 2, 2) should contain allOf(
          (0, 1), (0, 3),
          (1, 0), (1, 4),
          (3, 0), (3, 4),
          (4, 1), (4, 3)
          )
      }
    }

    "9x9 for (4,4)" should {
      val board: Board = new Board(9, 9)
      "have size 8" in {
        Knight.moves(board, 4, 4).size should be(8)
      }
      "have valid move coordinates" in {
        Knight.moves(board, 4, 4) should contain allOf(
          (2, 3), (2, 5),
          (3, 2), (3, 6),
          (5, 2), (5, 6),
          (6, 3), (6, 5)
          )
      }
    }
  }
}
