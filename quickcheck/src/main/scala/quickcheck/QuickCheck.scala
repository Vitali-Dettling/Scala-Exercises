package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = ???
  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  //Generator for maps of type Map[Int,Int].
  lazy val genMap: Gen[Map[Int,Int]] = for {
    k <- arbitrary[Int]
    v <- arbitrary[Int]
    m <- oneOf(const(Map.empty[Int,Int]), genMap)
  } yield m.updated(k, v)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  /**
    *  Insert an element into an empty heap,
    *  then find the minimum of the resulting heap,
    *  you get the element back.
    */
  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  //TODO:

  /**
    * If you insert any two elements into an empty heap, finding the minimum of the resulting heap should get the smallest of the two elements back.
    */

  /**
    * If you insert an element into an empty heap, then delete the minimum, the resulting heap should be empty.
    */

  /**
    * Given any heap, you should get a sorted sequence of elements when continually finding and deleting minima.
    * (Hint: recursion and helper functions are your friends.)
    */

  /**
    * Finding a minimum of the melding of any two heaps should return a minimum of one or the other.
    */
}
