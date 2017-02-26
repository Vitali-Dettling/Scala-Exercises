package examples

import org.scalacheck.Prop._

/**
  * Created by vitali on 2/26/17.
  */
trait ScalaCheckPass extends ScalaCheck{

  property("propConcatLists") = forAll { (l1: List[Int], l2: List[Int]) =>
  l1.size + l2.size == (l1 ::: l2).size }

}
