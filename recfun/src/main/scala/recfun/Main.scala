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
  def pascal(c: Int, r: Int): Int = {
    def pascalValue(cur: Int, n: Int): Int =
      if (cur == c) n
      else pascalValue(cur + 1, n * (r - cur) / (cur + 1))

    pascalValue(0, 1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def computeBracket(chars: List[Char], acc: Int): Boolean =
      if (chars.isEmpty)
        if (acc == 0) true else false
      else if (chars.head == '(') computeBracket(chars.tail, acc + 1)
      else if (chars.head == ')')
        if (acc > 0) computeBracket(chars.tail, acc - 1) else false
      else computeBracket(chars.tail, acc)

    computeBracket(chars, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    def countChange(money: Int, coins: List[Int]): Int =
      if (money < 0) 0
      else if (money == 0) 1
      else if (money >= 1 && coins.isEmpty) 0
      else countChange(money, coins.tail) + countChange(money - coins.head, coins)

    countChange(money, coins)
  }
}
