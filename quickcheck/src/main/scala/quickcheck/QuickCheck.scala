package quickcheck

import common._
import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  //Example: Generator for maps of type Map[Int,Int].
//  lazy val genMap: Gen[Map[Int,Int]] = for {
//    k <- arbitrary[Int]
//    v <- arbitrary[Int]
//    m <- oneOf(const(Map.empty[Int,Int]), genMap)
//  } yield m.updated(k, v)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  lazy val genHeap: Gen[H] = for {
    a: A <- arbitrary[A]
    b: A <- arbitrary[A]
    m: H <- oneOf(const(insert(a, empty)), genHeap)
  } yield insert(b,m)


  property("Find min delete min") = forAll { (hh: H) =>
    val h1 = findMin(hh)
    val h = findMin(deleteMin(hh))

    h1 != h
  }

  /**
    *  Count all elements
    *  */
  property("Count all elements") = forAll { (h1: H, h2: H) =>
    val h1c = count(h1)
    val h2c = count(h2)
    val m = meld(h1, h2)
    val hc = count(m)
    (h1c + h2c) == hc
  }

  def count(h: H): Int = {
    h match {
      case e if (isEmpty(e)) => 0
      case h: H => {
        1 + count(deleteMin(h))
      }
    }
  }

  /**
    * Is an empty block really empty?
    * */
  property("Is empty empty") = forAll { (a: A) =>
    isEmpty(empty)
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

  /**
    * If you insert any two elements into an empty heap,
    * finding the minimum of the resulting heap should get the smallest of the two elements back.
    */
  property("min2") = forAll { (a: Int, b: Int) =>
    val h1 = insert(a, empty)
    val h2 = insert(b, h1)
    val found = findMin(h2)
    val min = if(a < b) a else b
    min == found
  }

  /**
    * If you insert an element into an empty heap, then delete the minimum,
    * the resulting heap should be empty.
    */
  property("isEmpty") = forAll { (a: Int) =>
    val h = insert(a, empty)
    val del = deleteMin(h)
    isEmpty(del)
  }

  /**
    * Given any heap, you should get a sorted sequence of elements when continually finding and deleting minima.
    * (Hint: recursion and helper functions are your friends.)
    */
  property("sort delete min elem") = forAll { (h: H) =>
    val t = sortDelete(h)
    //Will exit after the first element is out of order.
    t.view.zip(t.tail).forall(x => x._1 <= x._2)
  }

  def sortDelete(h: H): List[A] = h match {
    case e if(isEmpty(e)) => Nil
    case h: H =>{
      val elem = findMin(h)
      elem :: sortDelete(deleteMin(h))
    }
  }

  /**
    * Finding a minimum of the melding of any two heaps should return a minimum of one or the other.
    */
  property("meld") = forAll { (h1: H, h2: H) =>
    val m = meld(h1, h2)
    val min = findMin(m)

    val min1 = findMin(h1)
    val min2 = findMin(h2)

    val trueMin = if(min1 < min2) min1 else min2

    trueMin == min
  }

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }
}
