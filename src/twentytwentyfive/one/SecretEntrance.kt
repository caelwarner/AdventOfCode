package twentytwentyfive.one

import util.kotlin.Part
import util.kotlin.Read
import util.kotlin.runPart
import kotlin.math.absoluteValue

fun main() {
    runPart(Part.ONE) { countTimesDialAt0(Read.asStringList("twentytwentyfive/one/input.txt")) }
    runPart(Part.TWO) { countTimesDialPasses0(Read.asStringList("twentytwentyfive/one/input.txt")) }
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
