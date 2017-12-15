package com.oodic.aoc

object Day15 {
  val input: (Long, Long) = (783, 325)

  def nextValue(value: Long, factor: Int, div: Int, mul: Int): Long = {
    val newValue = value * factor % div
    if (newValue % mul == 0) newValue
    else nextValue(newValue, factor, div, mul)
  }

  def resolve(
               input: (Long, Long),
               nPairs: Long,
               mulA: Int = 1,
               mulB: Int = 1,
               factorA: Int = 16807,
               factorB: Int = 48271,
               div: Int = 2147483647
             ) =
    (0L until nPairs).foldLeft((input, 0)) {
      case (((a, b), result), _) =>
        val newA = nextValue(a, factorA, div, mulA)
        val newB = nextValue(b, factorB, div, mulB)
        ((newA, newB), result + (if ((newA & 0xFFFF) == (newB & 0xFFFF)) 1 else 0))
    }._2

  def resolveFirst(input: (Long, Long)) = resolve(input, 40000000)

  def resolveSecond(input: (Long, Long)) = resolve(input, 5000000, 4, 8)

  def main(args: Array[String]): Unit = {
    println(s"[first star] ${resolveFirst(input)}")
    println(s"[second star] ${resolveSecond(input)}")
  }
}
