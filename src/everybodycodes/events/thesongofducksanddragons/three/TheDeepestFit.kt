package everybodycodes.events.thesongofducksanddragons.three

import util.kotlin.Part
import util.kotlin.Read
import util.kotlin.runPart

fun main() {
    runPart(Part.ONE) { maxSetOfCrates(Read.asString("everybodycodes/events/thesongofducksanddragons/three/input1.txt")) }
    runPart(Part.TWO) { minSetOf20Crates(Read.asString("everybodycodes/events/thesongofducksanddragons/three/input2.txt")) }
    runPart(Part.THREE) { minSetsOfCrates(Read.asString("everybodycodes/events/thesongofducksanddragons/three/input3.txt")) }
}

private fun maxSetOfCrates(input: String): Int =
    input.split(',').map(String::toInt).buildAscendingSet().sum()

private fun minSetOf20Crates(input: String): Int =
    input.split(',').map(String::toInt).buildAscendingSet().take(20).sum()

private fun minSetsOfCrates(input: String): Int {
    val crates = input.split(',').map(String::toInt).toMutableList()
    var sets = 0
    
    while (crates.isNotEmpty()) {
        crates.buildAscendingSet().forEach { crates.remove(it) }
        sets++
    }

    return sets
}

private fun List<Int>.buildAscendingSet(): Sequence<Int> =
    generateSequence(0) { current ->
        this.filter { it > current }.minOrNull()
    }
        .drop(1)
