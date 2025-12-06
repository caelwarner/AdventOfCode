package twentytwentyfive.six

import util.kotlin.Part
import util.kotlin.Read
import util.kotlin.runPart
import util.kotlin.splitAt
import util.kotlin.transpose

fun main() {
    runPart(Part.ONE) { solveCephalopodHomework(Read.asStringList("twentytwentyfive/six/input.txt")) }
    runPart(Part.TWO) { solveCephalopodHomeworkCorrectly(Read.asStringList("twentytwentyfive/six/input.txt")) }
}

private fun solveCephalopodHomework(input: List<String>): Long =
    input
        .map { it.trim().split("\\s+".toRegex()) }
        .transpose()
        .sumOf(::solveCephalopodEquation)

private fun solveCephalopodHomeworkCorrectly(input: List<String>): Long =
    input
        .last()
        .mapIndexedNotNull { i, c -> (i - 1).takeIf { c != ' ' } }
        .drop(1)
        .let { indices ->
            input
                .map { it.splitAt(indices) }
                .transpose()
                .map { it.dropLast(1).map { it.toList() }.transpose().map { it.joinToString("") } + it.last() }
                .sumOf(::solveCephalopodEquation)
        }

private fun solveCephalopodEquation(equation: List<String>): Long =
    equation
        .mapNotNull { it.trim().toLongOrNull() }
        .reduce { acc, i -> if (equation.last().trim() == "+") acc + i else acc * i }
