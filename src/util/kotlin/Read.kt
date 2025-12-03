package util.kotlin

import java.io.File

object Read {
    fun asStringList(filename: String): List<String> =
        File("src/$filename").readLines()

    fun asString(filename: String): String =
        File("src/$filename").readText()
}