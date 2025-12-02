package twentytwentyfive.one

import util.kotlin.Read
import kotlin.math.absoluteValue

fun main() {
    println("Part 1: ${countTimesDialAt0(Read.asStringList("src/twentytwentyfive/one/input.txt"))}")
    println("Part 2: ${countTimesDialPasses0(Read.asStringList("src/twentytwentyfive/one/input.txt"))}")
}

private fun countTimesDialAt0(input: List<String>): Int =
    input.map { it.drop(1).toInt() * if (it.first() == 'L') -1 else 1 }
        .runningFold(50) { acc, i -> (acc + i).mod(100) }
        .count { it == 0 }

private fun countTimesDialPasses0(input: List<String>): Int =
    input.map { it.drop(1).toInt() * if (it.first() == 'L') -1 else 1 }
        .runningFold(50 to 0) {
            (pos, _), delta -> (pos + delta).let { newPos -> Pair(
                newPos.mod(100),
                if (newPos <= 0 && pos != 0) newPos - 100 else newPos
            ) }
        }
        .sumOf { (_, i) -> (i / 100).absoluteValue }
