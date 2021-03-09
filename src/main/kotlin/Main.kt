import java.util.regex.Pattern


fun main(args: Array<String>) {


//    var mainStr = "\\frac{\\frac{1+1}{20+20}}{\\frac{10+10}{200+200}}+\\frac{3+3}{4+4}"
//    val result = mainStr.findAllStartEndIndexes("\\frac{", "}", 2)
//
//    val mutableList = mutableListOf<Pair<String, String>>()
//    result.reversed().forEach {
//        mutableList.add(pair( mainStr.getFromTo(it.first, it.second)))
//    }
//    mutableList.reversed().forEach {
//     mainStr =  mainStr.replace(it.first, it.second)
//    }

    val mainStr = "\\frac{\\frac{1+1}{20+20}}{\\frac{10+10}{200+200}}"
    println(test(mainStr))
//    frac()

}

//private fun frac() {
//    val list = mutableListOf<String>()
//
//    var result = "\\frac{1+1}{2+2} + \\frac{3+3}{4+4}"
//
//
//
//    result.split("+", "-", "*").forEach {
//        var command = it
//        while (true) {
//            if (command.contains("\\frac{")) {
//                command = pair(command)
//            } else {
//                break
//            }
//        }
//        if (command != "\\frac") {
//            list.add(command)
//        }
//    }
//
//    System.out.println(
//        "Result = ${list.joinToString("").replace(" ", "")}"
//    )
//}

private fun test(str: String): String {
    var str = str
    var first = ""
    var second = ""
    val ext = Pattern.compile("\\\\frac\\{(.*)")
    val p = str.split(ext)
//    val m = p.matcher(str)
//    while (m.find()) {
//        first = m.group(0)
//        second = m.group(0)
//        second = second.removePrefix("{")
//        second = second.removeSuffix("}")
//    }
    return p.joinToString { it }
}

private fun pair(str: String): Pair<String, String> {
    var first = ""
    var second = ""
    val p = Pattern.compile("(\\\\frac\\{)(.*)\\}(\\{(.*)\\})")
    val m = p.matcher(str)
    while (m.find()) {
        first = m.group(2)
        second = m.group(3)
        second = second.removePrefix("{")
        second = second.removeSuffix("}")
    }
    return Pair("\\frac{$first}{$second}", "($first)/($second)")
}

private fun String.getFromTo(startIndex: Int, endIndex: Int): String {
    var result = ""
    forEachIndexed { index: Int, char: Char ->
        if (index in startIndex..endIndex) {
            result += char
        }
    }
    return result
}

private fun String.findAllStartEndIndexes(start: String, end: String, countOfRepeat: Int): List<Pair<Int, Int>> {
    val firstsIndexValue = this.allIndexOf(start)
    val endsIndexValue = this.endsIndexOf(firstsIndexValue, start, end, countOfRepeat)
    return combineListsToPair(firstsIndexValue, endsIndexValue)
}

private fun combineListsToPair(firstsIndexValue: List<Int>, endsIndexValue: List<Int>): List<Pair<Int, Int>>{
    val mutableList = mutableListOf<Pair<Int, Int>>()
    firstsIndexValue.forEachIndexed { index, i ->
        mutableList.add(Pair(i, endsIndexValue[index]))
    }
    return mutableList
}

private fun String.endsIndexOf(
    firstsIndexValue: List<Int>,
    start: String,
    end: String,
    countOfRepeat: Int
): List<Int> {
    val mutableList = mutableListOf<Int>()
    val negativeValue = if (end == "}") "{" else if (end == ")") "(" else ""
    var countOfMeet = 0
    var repeat = countOfRepeat
    val i = 0
    for (element in firstsIndexValue){
        val indexOfStart = element + start.length -1
         loop@ for (( index, char) in this.withIndex()){
            if (index > indexOfStart) {
                if (countOfMeet != -1) {
                    if (countOfMeet <= -2) countOfMeet = 0
                    if (char.toString() == end) {
                        countOfMeet--
                        if (index == this.length -1){
                            mutableList.add(index)
                        }
                    } else if (char.toString() == negativeValue) {
                        countOfMeet++
                    }

                } else {
                    repeat--
                    countOfMeet--

                    if (repeat == 0){
                        mutableList.add(index - 1)
                        repeat = countOfRepeat
                        break@loop
                    }
                }
            }
        }
    }
    return mutableList.toList()
}

private fun String.allIndexOf(value: String): List<Int> {
    val mutableList = mutableListOf<Int>()
    var index: Int = this.indexOf(value)
    while (index >= 0) {
        mutableList.add(index)
        index = this.indexOf(value, index + 1)
    }
    return mutableList.toList()
}