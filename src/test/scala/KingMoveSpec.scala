import org.scalatest.{ShouldMatchers, WordSpec}
import problem.Chess.{King, Board}

class KingMoveSpec extends WordSpec with ShouldMatchers {

  "King Move" when {
    "1x1" should {
      val board: Board = new Board(1, 1)
      "should be empty" in {
        King.moves(board, 0, 0) should be(empty)
      }
    }

    "1x2 for (0,0)" should {
      val board: Board = new Board(1, 2)
      "have size 1" in {
        King.moves(board, 0, 0).size should be(1)
      }
      "have move coordinate just (0,1)" in {
        King.moves(board, 0, 0) should contain (0, 1)
      }
    }

    "1x2 for (0,1)" should {
      val board: Board = new Board(1, 2)
      "have size 1" in {
        King.moves(board, 0, 1).size should be(1)
      }
      "have coordinate just (0,0)" in {
        King.moves(board, 0, 1) should contain (0, 0)
      }
    }

    "3x3 for (1,1)" should {
      val board: Board = new Board(3, 3)
      "have size 8" in {
        King.moves(board, 1, 1).size should be(8)
      }
      "have valid move coordinates" in {
        King.moves(board, 1, 1) should contain allOf(
          (0, 0), (0, 1), (0, 2),
          (1, 0), /*1,1*/ (1, 2),
          (2, 0), (2, 1), (2, 2)
          )
      }
    }

    "3x3 for (2,2)" should {
      val board: Board = new Board(3, 3)
      "have size 3" in {
        King.moves(board, 2, 2).size should be(3)
      }
      "have valid move coordinates" in {
        King.moves(board, 2, 2) should contain allOf(
          (1, 1), (1, 2), (2, 1)
          )
      }
    }

    "10x10 for (1,1)" should {
      val board: Board = new Board(10, 10)
      "have size 8" in {
        King.moves(board, 1, 1).size should be(8)
      }
      "have valid move coordinates" in {
        King.moves(board, 1, 1) should contain allOf(
          (0, 0), (0, 1), (0, 2),
          (1, 0), /*1,1*/ (1, 2),
          (2, 0), (2, 1), (2, 2)
          )
      }
    }

    "10x10 for (5,7)" should {
      val board: Board = new Board(10, 10)
      "have size 8" in {
        King.moves(board, 5, 7).size should be(8)
      }
      "have valid move coordinates" in {
        King.moves(board, 5, 7) should contain allOf(
          (4, 6), (4, 7), (4, 8),
          (5, 6), /*5,7*/ (5, 8),
          (6, 6), (6, 7), (6, 8)
          )
      }
    }

  }
}
