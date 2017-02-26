package examples

import common._
import org.scalacheck.Prop.forAll

/**
  * Created by vitali on 2/26/17.
  */
// https://github.com/rickynils/scalacheck/blob/master/doc/UserGuide.md
object ScalaCheck {


  val propConcatLists = forAll { (l1: List[Int], l2: List[Int]) =>
    l1.size + l2.size == (l1 ::: l2).size }


  val propSqrt = forAll { (n: Int) => scala.math.sqrt(n*n) == n }

}
