package recfun

object RecFun extends RecFunInterface {

  def main(args: Array[String]): Unit = {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(s"${pascal(col, row)} ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = 
    if (c >  r)
    0 
    else if (c == 0 || c == r)
      1
    else 
      pascal(c - 1, r - 1) + pascal(c, r - 1) 


  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def helper(cntOpening: Int, chs: List[Char]): Boolean = {
      chs match {
        case hd::tail => if (hd == '(') (helper(cntOpening + 1, tail))
                         else if (hd == ')') (if (cntOpening <= 0) false else helper(cntOpening - 1, tail))
                         else helper(cntOpening, tail)
        case Nil => cntOpening == 0
      }
    }
    helper(0, chars)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    val coinsSorted = coins.sorted(Ordering[Int].reverse);
    def helper(mon: Int, coins: List[Int]): Int = {
      coins match {
        case hd::tail => (0 to mon/hd).foldLeft(0)((l, r) => l + helper(mon - hd*r, tail))
        case Nil => if (mon == 0) 1 else 0
      }
    }
    helper(money, coinsSorted)
  }
}
