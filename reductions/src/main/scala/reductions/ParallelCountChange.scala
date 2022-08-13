package reductions

import org.scalameter.*

object ParallelCountChangeRunner:

  @volatile var seqResult = 0

  @volatile var parResult = 0

  val standardConfig = config(
    Key.exec.minWarmupRuns := 20,
    Key.exec.maxWarmupRuns := 40,
    Key.exec.benchRuns := 80,
    Key.verbose := false
  ) withWarmer (Warmer.Default())

  def main(args: Array[String]): Unit =
    val amount = 250
    val coins = List(1, 2, 5, 10, 20, 50)
    val seqtime = standardConfig measure {
      seqResult = ParallelCountChange.countChange(amount, coins)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential count time: $seqtime")

    def measureParallelCountChange(threshold: => ParallelCountChange.Threshold): Unit = try
      val fjtime = standardConfig measure {
        parResult = ParallelCountChange.parCountChange(amount, coins, threshold)
      }
      println(s"parallel result = $parResult")
      println(s"parallel count time: $fjtime")
      println(s"speedup: ${seqtime.value / fjtime.value}")
    catch
      case e: NotImplementedError =>
        println("Not implemented.")

    println("\n# Using moneyThreshold\n")
    measureParallelCountChange(ParallelCountChange.moneyThreshold(amount))
    println("\n# Using totalCoinsThreshold\n")
    measureParallelCountChange(ParallelCountChange.totalCoinsThreshold(coins.length))
    println("\n# Using combinedThreshold\n")
    measureParallelCountChange(ParallelCountChange.combinedThreshold(amount, coins))

object ParallelCountChange extends ParallelCountChangeInterface :

  /** Returns the number of ways change can be made from the specified list of
   * coins for the specified amount of money.
   */
  def countChange(money: Int, coins: List[Int]): Int =
    def cch(money: Int, coins: List[Int]): Int =
      if coins.isEmpty then 0
      else if money == coins.head then 1
      else if money > coins.head then
        val diff = money - coins.head
        cch(diff, coins) + cch(money, coins.tail)
      else 0

    if money == 0 then 1
    else cch(money, coins.sorted)

  type Threshold = (Int, List[Int]) => Boolean

  /** In parallel, counts the number of ways change can be made from the
   * specified list of coins for the specified amount of money.
   */
  def parCountChange(money: Int, coins: List[Int], threshold: Threshold): Int =
    def seqCch(money: Int, coins: List[Int]): Int =
      if coins.isEmpty then 0
      else if money == coins.head then 1
      else if money > coins.head then
        val diff = money - coins.head
        seqCch(diff, coins) + seqCch(money, coins.tail)
      else 0

    def parallelCch(money: Int, coins: List[Int]): Int =
      if coins.isEmpty then 0
      else if money == coins.head then 1
      else if money > coins.head then
        if threshold(money, coins) then
          seqCch(money, coins)
        else
          val diff = money - coins.head
          val (change1, change2) = parallel(parallelCch(diff, coins), parallelCch(money, coins.tail))
          change1 + change2
      else 0

    if money == 0 then 1
    else parallelCch(money, coins.sorted)

  /** Threshold heuristic based on the starting money. */
  def moneyThreshold(startingMoney: Int): Threshold =
    (money, coins) => money <= startingMoney * 2 / 3

  /** Threshold heuristic based on the total number of initial coins. */
  def totalCoinsThreshold(totalCoins: Int): Threshold =
    (money, coins) => coins.length <= totalCoins * 2 / 3


  /** Threshold heuristic based on the starting money and the initial list of coins. */
  def combinedThreshold(startingMoney: Int, allCoins: List[Int]): Threshold =
    (money, coins) => money * coins.length <= startingMoney * allCoins.length / 2
