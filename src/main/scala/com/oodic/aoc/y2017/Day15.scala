package com.oodic.aoc.y2017

object Day15 extends Puzzle2017[(Long, Long), Int, Int] {
  override val input: (Long, Long) = (783, 325)

  val factorA = 16807
  val factorB = 48271
  val div = 2147483647

  def nextValue(value: Long, factor: Int, div: Int, mul: Int): Long = {
    val newValue = value * factor % div
    if (newValue % mul == 0) newValue
    else nextValue(newValue, factor, div, mul)
  }

  def resolve(input: (Long, Long), nPairs: Long, mulA: Int = 1, mulB: Int = 1): Int =
    (0L until nPairs).foldLeft((input, 0)) {
      case (((a, b), result), _) =>
        val newA = nextValue(a, factorA, div, mulA)
        val newB = nextValue(b, factorB, div, mulB)
        ((newA, newB), result + (if ((newA & 0xFFFF) == (newB & 0xFFFF)) 1 else 0))
    }._2

  override def resolveFirst(input: (Long, Long)): Int = resolve(input, 40000000)

  override def resolveSecond(input: (Long, Long)): Int = resolve(input, 5000000, 4, 8)
}
