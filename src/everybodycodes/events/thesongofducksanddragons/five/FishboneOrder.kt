package everybodycodes.events.thesongofducksanddragons.five

import util.kotlin.Part
import util.kotlin.Read
import util.kotlin.runPart

fun main() {
    runPart(Part.ONE) { swordQuality(Read.asString("everybodycodes/events/thesongofducksanddragons/five/input1.txt")) }
    runPart(Part.TWO) { qualityDifference(Read.asStringList("everybodycodes/events/thesongofducksanddragons/five/input2.txt")) }
    runPart(Part.THREE) { swordChecksum(Read.asStringList("everybodycodes/events/thesongofducksanddragons/five/input3.txt")) }
}

private fun swordQuality(input: String): Long {
    val fishbone = fishbone(input.substringAfter(':'))
    return swordQuality(fishbone)
}

private fun swordQuality(fishbone: List<List<Int?>>): Long =
    fishbone
        .map { it[1] }
        .joinToString("")
        .toLong()

private fun fishbone(input: String): List<List<Int?>> {
    val nums = input.split(',').map(String::toInt)
    val fishbone = mutableListOf(mutableListOf(null, nums[0], null))

    outer@ for (num in nums.drop(1)) {
        for (segment in fishbone) {
            if (segment[0] == null && num < (segment[1] ?: 0)) {
                segment[0] = num
                continue@outer
            } else if (segment[2] == null && num > (segment[1] ?: 0)) {
                segment[2] = num
                continue@outer
            }
        }
        
        fishbone.add(mutableListOf(null, num, null))
    }
    
    return fishbone.map { it.toList() }
}

private fun qualityDifference(input: List<String>): Long {
    val swords = input.map(::swordQuality)
    return swords.max() - swords.min()
}

private fun swordChecksum(input: List<String>): Int =
    input
        .map { it.substringBefore(':').toInt() to fishbone(it.substringAfter(':')) }
        .sortedWith { (id1, f1), (id2, f2) ->
            when {
                swordQuality(f1) > swordQuality(f2) -> -1
                swordQuality(f1) < swordQuality(f2) -> 1
                else -> {
                    for ((seg1, seg2) in f1.zip(f2)) {
                        val seg1 = seg1.filterNotNull().joinToString("").toLong()
                        val seg2 = seg2.filterNotNull().joinToString("").toLong()
                        
                        if (seg1 > seg2) return@sortedWith -1
                        else if (seg1 < seg2) return@sortedWith 1
                    }

                    return@sortedWith id2 - id1
                }
            }
        }
        .mapIndexed { i, (id, _) -> (i + 1) * id }
        .sum()
