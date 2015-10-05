import org.scalatest._
import problem.Chess
import problem.Chess._

class QueenMoveSpec extends WordSpec with ShouldMatchers {

  "Queen Move" when {

    "1x1" should {
      val board: Board = new Board(1, 1)
      "should be empty" in {
        Queen.moves(board, 0, 0) should be(empty)
      }
    }

    "3x3 for (1,1)" should {
      val board: Board = new Board(3, 3)
      "have size 8" in {
        Queen.moves(board, 1, 1).size should be(8)
      }
      "have valid move coordinates" in {
        Queen.moves(board, 1, 1) should contain allOf(
          (0, 0), (0, 1), (0, 2),
          (1, 0), /*1,1*/ (1, 2),
          (2, 0), (2, 1), (2, 2)
          )
      }
    }
  }
}
