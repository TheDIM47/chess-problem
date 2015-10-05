import org.scalatest._
import problem.Chess.{Rook, Board}

class RookMoveSpec extends WordSpec with ShouldMatchers {
  "Rook Move" when {

    "1x1" should {
      val board: Board = new Board(1, 1)
      "should be empty" in {
        Rook.moves(board, 0, 0) should be(empty)
      }
    }

    "2x2" should {
      val board: Board = new Board(2, 2)
      "should be 2" in {
        Rook.moves(board, 0, 0).size should be(2)
      }
      "have valid move coordinates" in {
        Rook.moves(board, 0, 0) should contain allOf((0, 1), (1, 0))
      }
    }

    "3x3 for (0,0)" should {
      val board: Board = new Board(3, 3)
      "should be 4" in {
        Rook.moves(board, 0, 0).size should be(4)
      }
      "have valid move coordinates" in {
        Rook.moves(board, 0, 0) should contain allOf(
          (0, 1), (0, 2),
          (1, 0), (2, 0)
          )
      }
    }

    "3x3 for (2,2)" should {
      val board: Board = new Board(3, 3)
      "should be 4" in {
        Rook.moves(board, 2, 2).size should be(4)
      }
      "have valid move coordinates" in {
        Rook.moves(board, 2, 2) should contain allOf(
          (2, 1), (2, 0),
          (1, 2), (0, 2)
          )
      }
    }

    "3x3 for (2,1)" should {
      val board: Board = new Board(3, 3)
      "should be 4" in {
        Rook.moves(board, 2, 1).size should be(4)
      }
      "have valid move coordinates" in {
        Rook.moves(board, 2, 1) should contain allOf(
          (2, 0), (2, 2),
          (1, 1), (0, 1)
          )
      }
    }

    "3x3 for (1,1)" should {
      val board: Board = new Board(3, 3)
      "should be 4" in {
        Rook.moves(board, 1, 1).size should be(4)
      }
      "have valid move coordinates" in {
        Rook.moves(board, 1, 1) should contain allOf(
          (2, 1),
          (1, 0), (1, 2),
          (0, 1)
          )
      }
    }


  }
}