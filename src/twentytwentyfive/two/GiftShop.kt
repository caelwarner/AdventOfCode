package twentytwentyfive.two

import util.kotlin.Part
import util.kotlin.Read
import util.kotlin.runPart

fun main() {
    runPart(Part.ONE) { sumOfInvalidIds(Read.asString("twentytwentyfive/two/input.txt")) }
    runPart(Part.TWO) { sumOfInvalidIdsAdvanced(Read.asString("twentytwentyfive/two/input.txt")) }
}

private fun sumOfInvalidIds(input: String): Long =
    input
        .split(Regex("[,-]"))
        .map { it.toLong() }
        .chunked(2)
        .flatMap { (a, b) -> (a..b) }
        .filter { it.toString().hasDoubledPattern() }
        .sum()

private fun sumOfInvalidIdsAdvanced(input: String): Long =
    input
        .split(Regex("[,-]"))
        .map { it.toLong() }
        .chunked(2)
        .flatMap { (a, b) -> (a..b) }
        .filter { it.toString().hasRepeatingPattern() }
        .sum()

private fun String.hasDoubledPattern(): Boolean =
    this.length % 2 == 0 && this.substring(0, this.length / 2) == this.substring(this.length / 2)

private fun String.hasRepeatingPattern(): Boolean =
    (1..this.length / 2)
        .filter { this.length % it == 0 }
        .map { this.take(it) }
        .map { it.repeat(this.length / it.length) == this }
        .any { it }
