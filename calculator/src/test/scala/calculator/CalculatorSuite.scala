package calculator

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import org.scalatest._

import TweetLength.MaxTweetLength

@RunWith(classOf[JUnitRunner])
class CalculatorSuite extends FunSuite with ShouldMatchers {


  /******************
    ** Calculator **
    ******************/
  test("computeValues"){

    val expr: Map[String, Signal[Expr]] = Map("a" -> Signal(Plus(Literal(2), Literal(2))))
    val res = Calculator.computeValues(expr)
    val ttt = res.map(p => p._2())
    assert(ttt.exists(p => p == 4.0))

  }

  test("computeValues with ref"){

    val expr: Map[String, Signal[Expr]] = Map(("a" -> Signal(Plus(Literal(2), Literal(2)))), ("b" -> Signal(Plus(Ref("a"), Literal(2)))))
    val res = Calculator.computeValues(expr)
    val ttt = res.map(p => p._2())
    assert(ttt.exists(p => p === 6.0))

  }

  /******************
    ** Polynomial **
    ******************/

  test("Math.sqrt(x)"){
    assert(Polynomial.sr(25) === 5)
  }

  //Δ = b² - 4ac
  test("computeDelta"){
    val res = Polynomial.computeDelta(Signal(1), Signal(5), Signal(1))
    assert(res() === 21)
  }

  //(-b ± √Δ) / 2a
  test("computeSolutions"){
    val res = Polynomial.computeSolutions(Signal(1), Signal(4), Signal(1), Signal(25))
    assert(res()(0.5))
    assert(res()(-4.5))
  }

  //(-b ± √Δ) / 2a
  test("delta is negative"){
    val res = Polynomial.computeSolutions(Signal(1), Signal(4), Signal(1), Signal(-25))
    assert(res()(0))
  }



  /******************
   ** TWEET LENGTH **
   ******************/

  def tweetLength(text: String): Int =
    text.codePointCount(0, text.length)

  test("tweetRemainingCharsCount with a constant signal") {
    val result = TweetLength.tweetRemainingCharsCount(Var("hello world"))
    assert(result() == MaxTweetLength - tweetLength("hello world"))

    val tooLong = "foo" * 200
    val result2 = TweetLength.tweetRemainingCharsCount(Var(tooLong))
    assert(result2() == MaxTweetLength - tweetLength(tooLong))
  }

  test("tweetRemainingCharsCount with a supplementary char") {
    val result = TweetLength.tweetRemainingCharsCount(Var("foo blabla \uD83D\uDCA9 bar"))
    assert(result() == MaxTweetLength - tweetLength("foo blabla \uD83D\uDCA9 bar"))
  }


  test("colorForRemainingCharsCount with a constant signal") {
    val resultGreen1 = TweetLength.colorForRemainingCharsCount(Var(52))
    assert(resultGreen1() == "green")
    val resultGreen2 = TweetLength.colorForRemainingCharsCount(Var(15))
    assert(resultGreen2() == "green")

    val resultOrange1 = TweetLength.colorForRemainingCharsCount(Var(12))
    assert(resultOrange1() == "orange")
    val resultOrange2 = TweetLength.colorForRemainingCharsCount(Var(0))
    assert(resultOrange2() == "orange")

    val resultRed1 = TweetLength.colorForRemainingCharsCount(Var(-1))
    assert(resultRed1() == "red")
    val resultRed2 = TweetLength.colorForRemainingCharsCount(Var(-5))
    assert(resultRed2() == "red")
  }

}
