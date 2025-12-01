package util.kotlin

import java.io.File

object Read {
    fun asStringList(filename: String): List<String> =
        File(filename).readLines()
}