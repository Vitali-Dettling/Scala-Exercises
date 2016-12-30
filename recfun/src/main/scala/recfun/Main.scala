package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
    * Exercise 1
    */
  //def pascal(c: Int, r: Int): Int = ???
  def pascal(c: Int, r: Int): Int =
  if (c == 0 || c == r) 1 else pascal(c, r - 1) + pascal(c - 1, r - 1)


  var countParentheses = List[String]()
  final var equalNumberOfParentheses = 0

  /**
    * Exercise 2
    */
  //def balance(chars: List[Char]): Boolean = ???
  def balance(chars: List[Char]): Boolean = {

    if (!chars.isEmpty) {

      if (chars.head == '(') countParentheses.mkString(")")
      if (chars.head == ')') countParentheses.mkString("(")

      balance(chars.tail)
    }

      if((countParentheses.length%2) == 0) true else false

    /* var exit = false
    var count = 0
    while(!exit){
      if(countParentheses(count) != "(" && countParentheses(count+1) == ")") count += 2 else exit = true
    }*/
  }


  var change = 0
  var count = 0
  var iteration = 0

  /**
    * Exercise 3
    * Taken from:
    * http://stackoverflow.com/questions/12629721/coin-change-algorithm-in-scala-using-recursion
    */
  //def countChange(money: Int, coins: List[Int]): Int = {
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0) {
      1
    }
    else if (money > 0 && !coins.isEmpty) {
      val mv1 = money - coins.head
      println("countChange(" + mv1.toString() + "," + coins + ")" + " + " + "countChange(" + money.toString() + "," + coins.tail + ")")
      countChange(money - coins.head, coins) + countChange(money, coins.tail)
    } else {
      0
    }
  }
}