package twentytwentyfive.three

import util.kotlin.Part
import util.kotlin.Read
import util.kotlin.runPart
import kotlin.math.pow

private val POWERS_OF_TEN = LongArray(12) { 10.0.pow(it).toLong() }

fun main() {
    runPart(Part.ONE) { sumBankJoltage2(Read.asStringList("twentytwentyfive/three/input.txt")) }
    runPart(Part.TWO) { sumBankJoltage12(Read.asStringList("twentytwentyfive/three/input.txt")) }
}

private fun sumBankJoltage2(input: List<String>): Int =
    input
        .map { bank -> bank
            .map { it.digitToInt() }
            .zipWithNext()
            .reduce { (f, l), (a, b) -> if (a > f) a to b else f to (if (b > l) b else l) }
        }
        .sumOf { (a, b) -> a * 10 + b }

private fun sumBankJoltage12(input: List<String>): Long =
    input
        .sumOf { bank ->
            (11 downTo 0)
            .fold(0 to 0L) { (start, acc), pos -> bank.maxJoltagePlace(pos, start).let { (i, d) -> i to d + acc } }
            .second
        }

private fun String.maxJoltagePlace(pos: Int, start: Int): Pair<Int, Long> =
    this
        .withIndex()
        .drop(start)
        .dropLast(pos)
        .maxBy { it.value.digitToInt() }
        .let { (i, d) -> i + 1 to d.digitToInt().toLong() * POWERS_OF_TEN[pos] }
