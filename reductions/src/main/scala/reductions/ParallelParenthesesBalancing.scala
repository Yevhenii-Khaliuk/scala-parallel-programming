package reductions

import org.scalameter.*

import scala.annotation.*

object ParallelParenthesesBalancingRunner:

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns := 40,
    Key.exec.maxWarmupRuns := 80,
    Key.exec.benchRuns := 120,
    Key.verbose := false
  ) withWarmer (Warmer.Default())

  def main(args: Array[String]): Unit =
    val length = 100000000
    val chars = new Array[Char](length)
    val threshold = 10000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime")
    println(s"speedup: ${seqtime.value / fjtime.value}")

object ParallelParenthesesBalancing extends ParallelParenthesesBalancingInterface :

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def balance(chars: Array[Char]): Boolean =
    @tailrec
    def bal(index: Int, acc: Int): Boolean =
      chars(index) match
        case '(' if index == chars.length - 1 => false
        case '(' => bal(index + 1, acc + 1)
        case ')' if acc == 0 => false
        case ')' if index == chars.length - 1 && acc == 1 => true
        case ')' => bal(index + 1, acc - 1)
        case _ if index == chars.length - 1 => acc == 0
        case _ => bal(index + 1, acc)

    if chars.isEmpty then true
    else bal(0, 0)

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean =

    @tailrec
    def traverse(idx: Int, until: Int, leftBrackets: Int, rightBrackets: Int): (Int, Int) = {
      if idx >= until then (leftBrackets, rightBrackets)
      else
        chars(idx) match
          case '(' => traverse(idx + 1, until, leftBrackets + 1, rightBrackets)
          case ')' =>
            if leftBrackets > 0 then traverse(idx + 1, until, leftBrackets - 1, rightBrackets)
            else traverse(idx + 1, until, leftBrackets, rightBrackets + 1)
          case _ => traverse(idx + 1, until, leftBrackets, rightBrackets)
    }

    def reduce(from: Int, until: Int): (Int, Int) = {
      if until - from <= threshold then traverse(from, until, 0, 0)
      else
        val mid = (until - from) / 2 + from
        val (left, right) = parallel(reduce(from, mid), reduce(mid, until))
        (left._1 - right._2 + right._1, left._2)
    }

    reduce(0, chars.length) == (0, 0)
