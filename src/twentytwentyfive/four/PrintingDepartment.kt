package twentytwentyfive.four

import util.kotlin.Part
import util.kotlin.Read
import util.kotlin.runPart
import util.kotlin.toInt
import util.kotlin.zip3

fun main() {
    runPart(Part.ONE) { countDirectlyAccessibleRollsOfPaper(Read.asStringList("twentytwentyfive/four/input.txt")) }
    runPart(Part.TWO) { countAccessibleRollsOfPaper(Read.asStringList("twentytwentyfive/four/input.txt")) }
}

private fun countDirectlyAccessibleRollsOfPaper(input: List<String>): Int =
    input.countPaper() - removeDirectlyAccessibleRowsOfPaper(input).countPaper()

private fun countAccessibleRollsOfPaper(input: List<String>): Int =
    generateSequence(input) { grid ->
        removeDirectlyAccessibleRowsOfPaper(grid).takeIf { it.countPaper() < grid.countPaper() }
    }
        .zipWithNext { prev, next -> prev.countPaper() - next.countPaper() }
        .sum()

private fun removeDirectlyAccessibleRowsOfPaper(input: List<String>): List<String> =
    (listOf(".".repeat(input[0].length)) + input + listOf(".".repeat(input[0].length)))
        .map { row ->
            (".$row.")
                .toList()
                .windowed(3)
                .map { (l, m, r) -> (l == '@').toInt() + (m == '@').toInt() + (r == '@').toInt() to m }
        }
        .windowed(3)
        .map { (tl, ml, bl) ->
            tl
                .zip3(ml, bl) { (tc, _), (mc, m), (bc, _) -> tc + mc + bc to m }
                .map { (d, c) -> if (c == '@' && d - 1 < 4) '.' else c }
                .joinToString("")
        }

private fun List<String>.countPaper(): Int =
    this.sumOf { it.count { c -> c == '@' } }
