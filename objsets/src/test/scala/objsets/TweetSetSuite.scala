package objsets

import org.scalatest.FunSuite


import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TweetSetSuite extends FunSuite {
  trait TestSets {
    val set1 = new Empty
    val a = new Tweet("a", "a body", 20)
    val set2 = set1.incl(new Tweet("a", "a body", 20))
    val set3 = set2.incl(new Tweet("b", "b body", 20))
    val c = new Tweet("c", "c body", 7)
    val d = new Tweet("d", "d body", 9)
    val set4c = set3.incl(c)
    val set4d = set3.incl(d)
    val set5 = set4c.incl(d)
    val set6 = set1.incl(c).incl(d).incl(a)
    val set6_1 = set1.incl(a).incl(d).incl(c)
    val set6_2 = set1.incl(c).incl(a).incl(d)
  }

  def asSet(tweets: TweetSet): Set[Tweet] = {
    var res = Set[Tweet]()
    tweets.foreach(res += _)
    res
  }

  def size(set: TweetSet): Int = asSet(set).size

  test("filter: on empty set") {
    new TestSets {
      assert(size(set1.filter(tw => tw.user == "a")) === 0)
    }
  }

  test("filter: a on set5") {
    new TestSets {
      var t = set5.filter(tw => tw.user == "a")
      var t2 = size(set5.filter(tw => tw.user == "a"))
      assert(t2 === 1)
    }
  }

  test("filter: 20 on set5") {
    new TestSets {
      var t = set5.filter(tw => tw.retweets == 20)
      var t2 = size(t)
      assert(t2 === 2)
    }
  }

  test("union: set4c and set4d") {
    new TestSets {
      assert(size(set4c.union(set4d)) === 4)
    }
  }

  test("union: with empty set (1)") {
    new TestSets {
      assert(size(set5.union(set1)) === 4)
    }
  }

  test("union: with empty set (2)") {
    new TestSets {
      assert(size(set1.union(set5)) === 4)
    }
  }

  test("descending: set6_* mostRetweeted") {
    new TestSets {
      val trends = set6.mostRetweeted
      val trends1 = set6_1.mostRetweeted
      val trends2 = set6_2.mostRetweeted
      val trends3 = set5.mostRetweeted

      assert(trends.user == "a")
      assert(trends1.user == "a")
      assert(trends2.user == "a")
      assert(trends3.user == "a")
    }
  }

  test("descending: set5 descendingByRetweet") {
    new TestSets {
      val trends = set5.descendingByRetweet
      assert(!trends.isEmpty)
      assert(trends.head.user == "a" || trends.head.user == "b")
    }
  }
}