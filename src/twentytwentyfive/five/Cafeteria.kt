package twentytwentyfive.five

import util.kotlin.Part
import util.kotlin.Read
import util.kotlin.runPart
import util.kotlin.union

fun main() {
    runPart(Part.ONE) { countFreshIngredients(Read.asStringList("twentytwentyfive/five/input.txt")) }
    runPart(Part.TWO) { countAllFreshIngredients(Read.asStringList("twentytwentyfive/five/input.txt")) }
}

private fun countFreshIngredients(input: List<String>): Int =
    parseRanges(input)
        .let { ranges -> input
            .drop(ranges.size + 1)
            .map { it.toLong() }
            .filter { ranges.any { range -> it in range } }
        }
        .count()

private fun countAllFreshIngredients(input: List<String>): Long =
    parseRanges(input)
        .sortedBy { it.first }
        .fold(emptyList<LongRange>()) { ranges, range ->
            (if (ranges.isEmpty()) listOf(range) else ranges.dropLast(1) + (ranges.last() union range))
        }
        .sumOf { it.last - it.first + 1 }


private fun parseRanges(input: List<String>): List<LongRange> =
    input
        .takeWhile { it != "" }
        .map { it.substringBefore('-').toLong()..it.substringAfter('-').toLong() }
