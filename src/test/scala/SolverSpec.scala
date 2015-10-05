import org.scalatest._
import problem.Chess
import problem.Chess._

class SolverSpec extends WordSpec with ShouldMatchers {

  "Solutions" when {

    "1x2 board with 1xKing" should {
      val figures = List(King)
      "should be non-empty and have 2 solutions" in {
        val solution = Chess.solver(1, 2, figures)
        solution shouldNot be(empty)
        solution.size should be (2)
        solution.foreach(println _)
      }
    }

    "1x3 board with 1xKing and 1xKnight" should {
      val figures = List(King, Knight)
      "should be non-empty and have 2 solutions" in {
        val solution = Chess.solver(1, 3, figures)
        solution shouldNot be(empty)
        solution.size should be (2)
        solution.foreach(println _)
      }
    }

    "1x3 board with 1xKing and 1xBishop" should {
      val figures = List(King, Bishop)
      "should be non-empty and have 2 solutions" in {
        val solution = Chess.solver(1, 3, figures)
        solution shouldNot be(empty)
        solution.size should be (2)
        solution.foreach(println _)
      }
    }

    "1x3 board with 1xKing and 1xKing" should {
      val figures = List(King, King)
      "should be non-empty and have 1 solutions" in {
        val solution = Chess.solver(1, 3, figures)
        solution shouldNot be(empty)
        solution.size should be (1)
        solution.foreach(println _)
      }
    }

    "1x3 board with 3xKnights" should {
      val figures = List(Knight, Knight, Knight)
      "should be non-empty and have 1 solution" in {
        val solution = Chess.solver(1, 3, figures)
        solution shouldNot be(empty)
        solution.size should be (1)
        solution.foreach(println _)
      }
    }

    "1x3 board with 3xBishops" should {
      val figures = List(Bishop, Bishop, Bishop)
      "should be non-empty and have 1 solution" in {
        val solution = Chess.solver(1, 3, figures)
        solution shouldNot be(empty)
        solution.size should be (1)
        solution.foreach(println _)
      }
    }

    "3x3 board with 2xKings and 1xRooks" should {
      val figures = List(King, King, Rook)
      "should be non-empty and have 4 solutions" in {
        val solution = Chess.solver(3, 3, figures)
        solution shouldNot be(empty)
        solution.size should be (4)
        solution.foreach(println _)
      }
    }

    "4×4 board containing 2 Rooks and 4 Knights" should {
      val figures = List(Rook, Knight, Knight, Rook, Knight, Knight)
      "should be non-empty and have 8 solutions" in {
        val solution = Chess.solver(4, 4, figures)
        solution shouldNot be(empty)
        solution.foreach(println _)
        solution.size should be (8)
      }
    }
  }
}
