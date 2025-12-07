package everybodycodes.events.thesongofducksanddragons.one

import util.kotlin.Part
import util.kotlin.Read
import util.kotlin.runPart
import kotlin.math.max
import kotlin.math.min

fun main() {
    runPart(Part.ONE) { findOwnName(Read.asStringList("everybodycodes/events/thesongofducksanddragons/one/input1.txt")) }
    runPart(Part.TWO) { findFirstParentsName(Read.asStringList("everybodycodes/events/thesongofducksanddragons/one/input2.txt")) }
    runPart(Part.THREE) { findSecondParentsName(Read.asStringList("everybodycodes/events/thesongofducksanddragons/one/input3.txt")) }
}

private fun findOwnName(input: List<String>): String =
    findName(input) { acc, x, names -> min(max(acc + x, 0), names.size - 1) }

private fun findFirstParentsName(input: List<String>): String =
    findName(input) { acc, x, names -> (acc + x).mod(names.size) }

private fun findSecondParentsName(input: List<String>): String =
    findName(input) { _, x, names ->
        val i = x.mod(names.size)
        names[0] = names[i].also { names[i] = names[0] }
        0
    }

private fun findName(input: List<String>, operation: (Int, Int, MutableList<String>) -> Int): String {
    val names = input[0].split(',').toMutableList()
    
    val i = input[2]
        .split(',')
        .map { it.replace("L", "-").replace("R", "").toInt() }
        .fold(0) { acc, x -> operation(acc, x, names) }
    
    return names[i]
}
