import org.scalatest._
import problem.Chess.{Rook, Board}

class BoardSpec extends WordSpec with ShouldMatchers {

  "Boards" when {

    "2 empty boards with same size" should {
      val a = new Board(3, 3)
      val b = new Board(3, 3)
      "have same hashCode" in {
        a.hashCode should be(b.hashCode)
        b.hashCode should be(a.hashCode)
      }
      "should be equals" in {
        a should be(b)
        b should be(a)
      }
    }

    "2 empty boards with same size and figure" should {
      val f = Rook
      val a = new Board(3, 3)
      val b = new Board(3, 3)
      a.trySet(1, 1, Rook)
      b.trySet(1, 1, Rook)
      "have same hashCode" in {
        a.hashCode should be(b.hashCode)
        b.hashCode should be(a.hashCode)
      }
      "should be equals" in {
        a should be(b)
        b should be(a)
      }
    }


  }

}
