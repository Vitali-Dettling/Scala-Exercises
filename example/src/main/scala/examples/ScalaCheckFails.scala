package examples

import org.scalacheck.Prop._

/**
  * Created by vitali on 2/26/17.
  */
trait ScalaCheckFails extends ScalaCheck{

  property("propSqrt") = forAll { (n: Int) => scala.math.sqrt(n*n) == n }

}
