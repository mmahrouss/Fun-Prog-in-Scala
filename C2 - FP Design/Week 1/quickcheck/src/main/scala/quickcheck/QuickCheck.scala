package quickcheck

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    element <- arbitrary[Int]
    intHeap <- oneOf(empty, insert(element, empty))
  } yield intHeap
  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("min2") = forAll { (a: Int, b: Int, c: Int, d: Int) =>
    val h = insert(
      (a).abs + (b).abs + (c).abs + (d).abs,
      meld(insert(d, insert(c, empty)), insert(b, insert(a, empty)))
    )
    findMin(
      h
    ) == (a min b min c min d min ((a).abs + (b).abs + (c).abs + (d).abs))
  }

  property("meld1") = forAll { (a: Int, b: Int, c: Int, d: Int) =>
    val h = meld(insert(d, insert(c, empty)), insert(b, insert(a, empty)))
    findMin(h) == (a min b min c min d)
  }

  property("delmin1") = forAll { (a: Int) =>
    val (b, c, d) = (a + 1, a + 2, a + 3)
    val h = insert(
      (a).abs + (b).abs + (c).abs + (d).abs,
      meld(insert(d, insert(c, empty)), insert(b, insert(a, empty)))
    )
    val minA = (a min b min c min d min ((a).abs + (b).abs + (c).abs + (d).abs))
    findMin(deleteMin(h)) != minA
  }
}
