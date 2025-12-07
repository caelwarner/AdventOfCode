package everybodycodes.events.thesongofducksanddragons.four

import util.kotlin.Part
import util.kotlin.Read
import util.kotlin.runPart

fun main() {
    runPart(Part.ONE) { calibrate1stMill(Read.asStringList("everybodycodes/events/thesongofducksanddragons/four/input1.txt")) }
    runPart(Part.TWO) { calibrate2ndMill(Read.asStringList("everybodycodes/events/thesongofducksanddragons/four/input2.txt")) }
    runPart(Part.THREE) { calibrate3rdMill(Read.asStringList("everybodycodes/events/thesongofducksanddragons/four/input3.txt")) }
}

private fun calibrate1stMill(input: List<String>): Int {
    val gears = input.map(String::toInt)
    
    return 2025 * gears.first() / gears.last()
}

private fun calibrate2ndMill(input: List<String>): Long {
    val gears = input.map(String::toLong)

    return (10000000000000L * gears.last()).ceilDiv(gears.first())
}

private fun calibrate3rdMill(input: List<String>): Long {
    val input = input.toMutableList()
    input[0] = "${input[0]}|${input[0]}"
    input[input.size - 1] = "${input[input.size - 1]}|${input[input.size - 1]}"
    
    return input
        .map { it.substringBefore('|').toDouble() to it.substringAfter('|').toDouble() }
        .zipWithNext()
        .fold(100.0) { acc, (a, b) -> acc * a.second / b.first }
        .toLong()
}
    
private fun Long.ceilDiv(other: Long): Long =
    (this + other - 1) / other
