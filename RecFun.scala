package recfun
import scala.annotation.tailrec
object RecFun extends RecFunInterface:

  def main(args: Array[String]): Unit =
    println("Pascal's Triangle")
    for row <- 0 to 10 do
      for col <- 0 to row do
        print(s"${pascal(col, row)} ")
      println()

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {

    @tailrec
    def fact(n: Int, f: BigInt = 1): BigInt = if (n == 0) f else fact(n - 1, n * f)

    (fact(r) / (fact(c) * fact(r - c))).intValue
  }



  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    @tailrec
    def balanceInternal(remainChars: List[Char], numberOfOpenedBraces: Int): Boolean = {
      def calcSymValue(sym: Char): Int =
        sym match {
          case '(' => 1
          case ')' => -1
          case _ => 0
        }

      if (numberOfOpenedBraces < 0) false
      else if (remainChars.isEmpty) numberOfOpenedBraces == 0
      else balanceInternal(remainChars.tail, numberOfOpenedBraces + calcSymValue(remainChars.head))
    }

    balanceInternal(chars, 0)
  }


  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    @tailrec
    def countChangeInternal(money: Int, coins: List[Int], curValue: Int): Int =
      if (money == 0) curValue + 1
      else if (coins.isEmpty || money < 0) curValue
      else countChangeInternal(money - coins.head, coins, curValue + countChange(money, coins.tail))

    countChangeInternal(money, coins, 0)
  }


