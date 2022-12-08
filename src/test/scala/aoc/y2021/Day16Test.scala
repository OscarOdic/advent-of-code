package aoc.y2021

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day16Test extends AnyFlatSpec with Matchers {
  "Day16 - 2021" should "solve first part" in {
    val testInput1 = "8A004A801A8002F478"
    val testInput2 = "620080001611562C8802118E34"
    val testInput3 = "C0015000016115A2E0802F182340"
    val testInput4 = "A0016C880162017C3686B18A3D4780"

    Day16.part1(testInput1) should equal(16)
    Day16.part1(testInput2) should equal(12)
    Day16.part1(testInput3) should equal(23)
    Day16.part1(testInput4) should equal(31)

    Day16.part1(Day16.input) should equal(901)
  }

  it should "solve second part" in {
    val testInput1 = "C200B40A82"
    val testInput2 = "04005AC33890"
    val testInput3 = "880086C3E88112"
    val testInput4 = "CE00C43D881120"
    val testInput5 = "D8005AC2A8F0"
    val testInput6 = "F600BC2D8F"
    val testInput7 = "9C005AC2F8F0"
    val testInput8 = "9C0141080250320F1802104A08"

    Day16.part2(testInput1) should equal(3)
    Day16.part2(testInput2) should equal(54)
    Day16.part2(testInput3) should equal(7)
    Day16.part2(testInput4) should equal(9)
    Day16.part2(testInput5) should equal(1)
    Day16.part2(testInput6) should equal(0)
    Day16.part2(testInput7) should equal(0)
    Day16.part2(testInput8) should equal(1)

    Day16.part2(Day16.input) should equal(110434737925L)
  }
}
