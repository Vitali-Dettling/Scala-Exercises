package examples

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
  * Created by vitali on 2/26/17.
  */
// https://github.com/rickynils/scalacheck/blob/master/doc/UserGuide.md
@RunWith(classOf[JUnitRunner])
class ScalaCheckSuit extends FunSuite {

  import ScalaCheck._



    test("1"){
      print(propConcatLists.check.toString())
    }

  /**
    * Not surprisingly, the property doesn't hold. The argument -1 falsifies it.
    * You can also see that the argument -488187735 falsifies the property.
    * That was the first argument ScalaCheck found, and it was then simplified to -1.
    * You'll read more about this later on.
    * */
    test("2"){
      print(propSqrt.check.toString())
    }




}
