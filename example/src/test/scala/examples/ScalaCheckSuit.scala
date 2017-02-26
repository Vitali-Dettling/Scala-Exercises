package examples

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalacheck.Prop
import org.scalatest.exceptions.TestFailedException
import org.scalatest.junit.JUnitRunner
import org.scalatest.prop.Checkers


/**
  * Created by vitali on 2/26/17.
  */
// https://github.com/rickynils/scalacheck/blob/master/doc/UserGuide.md
@RunWith(classOf[JUnitRunner])
class ScalaCheckSuit extends FunSuite with Checkers {

  def checkBogus(p: Prop) {
    var ok = false
    try {
      check(p)
    } catch {
      case e: TestFailedException =>
        ok = true
    }
    assert(ok, "It is suppose to fail.")
  }

    test("Properties check"){
      check(new ScalaCheck with ScalaCheckPass)
    }

  /**
    * Not surprisingly, the property doesn't hold. The argument -1 falsifies it.
    * You can also see that the argument -488187735 falsifies the property.
    * That was the first argument ScalaCheck found, and it was then simplified to -1.
    * You'll read more about this later on.
    * */
    test("2"){
      checkBogus(new ScalaCheck with ScalaCheckFails)
    }




}
