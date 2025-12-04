package twentytwentyfive.four

import util.kotlin.Part
import util.kotlin.Read
import util.kotlin.runPart
import util.kotlin.toInt

fun main() {
    runPart(Part.ONE) { countDirectlyAccessibleRollsOfPaper(Read.asStringList("twentytwentyfive/four/input.txt")) }
    runPart(Part.TWO) { countAccessibleRollsOfPaper(Read.asStringList("twentytwentyfive/four/input.txt")) }
}

private fun countDirectlyAccessibleRollsOfPaper(input: List<String>): Int =
    input.countPaper() - removeDirectlyAccessibleRowsOfPaper(input).countPaper()

private fun countAccessibleRollsOfPaper(input: List<String>): Int =
    generateSequence(input to 1) { (grid, _) ->
        removeDirectlyAccessibleRowsOfPaper(grid).let { grid2 ->
            grid2 to grid.countPaper() - grid2.countPaper()
        }
    }
        .takeWhile { (_, removed) -> removed != 0 }
        .sumOf { (_, removed) -> removed } - 1

private fun removeDirectlyAccessibleRowsOfPaper(input: List<String>): List<String> =
    (listOf(".".repeat(input[0].length + 2)) + input + listOf(".".repeat(input[0].length + 2)))
        .map { row ->
            (".$row.")
                .toList()
                .windowed(3)
                .map { (l, m, r) -> (l == '@').toInt() + (m == '@').toInt() + (r == '@').toInt() to m }
        }
        .windowed(3)
        .map { (tl, ml, bl) ->
            tl
                .zip(ml)
                .map { (t, m) -> t.first + m.first to m.second }
                .zip(bl)
                .map { (m, b) -> m.first + b.first to m.second }
                .map { (d, c) -> if (c == '@' && d - 1 < 4) '.' else c }
                .joinToString("")
        }

private fun List<String>.countPaper(): Int =
    this.sumOf { it.count { c -> c == '@' } }
