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
        solution.size should be(2)
        solution.foreach(x => Chess.printBoard(1, 2, x))
        //        solution.foreach(println _)
      }
    }

    "1x3 board with 1xKing and 1xKnight" should {
      val figures = List(King, Knight)
      "should be non-empty and have 2 solutions" in {
        val solution = Chess.solver(1, 3, figures)
        solution shouldNot be(empty)
        solution.size should be(2)
        solution.foreach(x => Chess.printBoard(1, 3, x))
        //        solution.foreach(println _)
      }
    }

    "1x3 board with 1xKing and 1xBishop" should {
      val figures = List(King, Bishop)
      "should be non-empty and have 2 solutions" in {
        val solution = Chess.solver(1, 3, figures)
        solution shouldNot be(empty)
        solution.size should be(2)
        solution.foreach(x => println(Chess.printBoard(1, 3, x)))
        //        solution.foreach(println _)
      }
    }

    "1x3 board with 1xKing and 1xKing" should {
      val figures = List(King, King)
      "should be non-empty and have 1 solutions" in {
        val solution = Chess.solver(1, 3, figures)
        solution shouldNot be(empty)
        solution.size should be(1)
        solution.foreach(x => println(Chess.printBoard(1, 3, x)))
        //        solution.foreach(println _)
      }
    }

    "1x3 board with 3xKnights" should {
      val figures = List(Knight, Knight, Knight)
      "should be non-empty and have 1 solution" in {
        val solution = Chess.solver(1, 3, figures)
        solution shouldNot be(empty)
        solution.size should be(1)
        solution.foreach(x => println(Chess.printBoard(1, 3, x)))
        //        solution.foreach(println _)
      }
    }

    "1x3 board with 3xBishops" should {
      val figures = List(Bishop, Bishop, Bishop)
      "should be non-empty and have 1 solution" in {
        val solution = Chess.solver(1, 3, figures)
        solution shouldNot be(empty)
        solution.size should be(1)
        solution.foreach(x => println(Chess.printBoard(1, 3, x)))
        //        solution.foreach(println _)
      }
    }

    "3x3 board with 2xKings and 1xRooks" should {
      val figures = List(King, King, Rook)
      "should be non-empty and have 4 solutions" in {
        val solution = Chess.solver(3, 3, figures)
        solution shouldNot be(empty)
        solution.size should be(4)
        solution.foreach(x => println(Chess.printBoard(3, 3, x)))
        //        solution.foreach(println _)
      }
    }

    "4×4 board containing 2 Rooks and 4 Knights" should {
      val figures = List(Rook, Knight, Knight, Rook, Knight, Knight)
      "should be non-empty and have 8 solutions" in {
        val solution = Chess.solver(4, 4, figures)
        solution shouldNot be(empty)
        solution.size should be(8)
        solution.foreach(x => println(Chess.printBoard(4, 4, x)))
        //        solution.foreach(println _)
      }
    }

    "5×5 board containing 3 Rooks and 4 Knights" should {
      val figures = List(Rook, Knight, Knight, Rook, Knight, Knight, Rook)
      "should be non-empty and have ? solutions" in {
        val solution = Chess.solver(5, 5, figures)
        //        solution shouldNot be(empty)
        solution.foreach(x => println(Chess.printBoard(5, 5, x)))
        solution.foreach(println _)
      }
    }

    "5×6 board containing 3 Rooks and 5 Knights" should {
      val figures = List(Rook, Knight, Knight, Rook, Knight, Knight, Rook, Knight)
      "should be non-empty and have ? solutions" in {
        val solution = Chess.solver(5, 6, figures)
        //        solution shouldNot be(empty)
        solution.foreach(x => println(Chess.printBoard(5, 6, x)))
        solution.foreach(println _)
      }
    }

    "5x8 board with 2 Kings, 1 Queen, 1 Bishop, 1 Rook and 1 Knight" should {
      val figures = List(King, King, Queen, Bishop, Rook, Knight)
      "should be non-empty and have N solutions" in {
        val solution = Chess.solver(5, 8, figures)
//        solution shouldNot be(empty)
        println(solution.size)
        solution.foreach(x => Chess.printBoard(5, 8, x))
//        solution.foreach(println _)
      }
    }


    //    "6x9 board with 2 Kings, 1 Queen, 1 Bishop, 1 Rook and 1 Knight" should {
    //      val figures = List(King, King, Queen, Bishop, Rook, Knight)
    //      "should be non-empty and have N solutions" in {
    //        val solution = Chess.solver(6, 9, figures)
    //        solution shouldNot be(empty)
    //        println(solution.size)
    ////        solution.foreach(x => Chess.printBoard(6, 9, x))
    //        solution.foreach(println _)
    //      }
    //    }

  }
}
