object DynamicCalculator {

    // New dynamic calculate function
    fun calculate(str: String): Double {
        return object : Any() {
            var pos = -1
            var ch = 0
            fun nextChar() {
                ch = if (++pos < str.length) str[pos].toInt() else -1
            }

            fun eat(charToEat: Int): Boolean {
                while (ch == ' '.toInt()) nextChar()
                if (ch == charToEat) {
                    nextChar()
                    return true
                }
                return false
            }

            fun parse(): Double {
                nextChar()
                val x = parseExpression()
                if (pos < str.length) throw RuntimeException("Unexpected: " + ch.toChar())
                return x
            }

            // Grammar:
            // expression = term | expression `+` term | expression `-` term
            // term = factor | term `*` factor | term `/` factor
            // factor = `+` factor | `-` factor | `(` expression `)`
            //        | number | functionName factor | factor `^` factor
            fun parseExpression(): Double {
                var x = parseTerm()
                while (true) {
                    when {
                        eat('+'.toInt()) -> x += parseTerm() // addition
                        eat('-'.toInt()) -> x -= parseTerm() // subtraction
                        else -> return x
                    }
                }
            }

            fun parseTerm(): Double {
                var x = parseFactor()
                while (true) {
                    when {
                        eat('*'.toInt()) -> x *= parseFactor() // multiplication
                        eat('/'.toInt()) -> x /= parseFactor() // division
                        else -> return x
                    }
                }
            }

            fun parseFactor(): Double {
                if (eat('+'.toInt())) return parseFactor() // unary plus
                if (eat('-'.toInt())) return -parseFactor() // unary minus
                var x: Double
                val startPos = pos
                if (eat('('.toInt())) { // parentheses
                    x = parseExpression()
                    eat(')'.toInt())
                } else if (ch >= '0'.toInt() && ch <= '9'.toInt() || ch == '.'.toInt()) { // numbers
                    while (ch >= '0'.toInt() && ch <= '9'.toInt() || ch == '.'.toInt()) nextChar()
                    x = str.substring(startPos, pos).toDouble()
                } else if (ch >= 'a'.toInt() && ch <= 'z'.toInt()) { // functions
                    while (ch >= 'a'.toInt() && ch <= 'z'.toInt()) nextChar()
                    val func = str.substring(startPos, pos)
                    x = parseFactor()
                    x = when (func) {
                        "sqrt" -> Math.sqrt(x)
                        "sin" -> Math.sin(Math.toRadians(x))
                        "cos" -> Math.cos(
                            Math.toRadians(x)
                        )
                        "tan" -> Math.tan(Math.toRadians(x))
                        else -> throw RuntimeException(
                            "Unknown function: $func"
                        )
                    }
                } else {
                    throw RuntimeException("Unexpected: " + ch.toChar())
                }
                if (eat('^'.toInt())) x = Math.pow(x, parseFactor()) // exponentiation
                return x
            }
        }.parse()
    }

    //Old dynamic calculate function
    fun eval(str: String): Double {
        //Удаляем внешние пробелы. Из-за рекурсии удаляются все пробелы, кроме таких "5 5+7"
        var str = str
        str = str.trim { it <= ' ' }
        // кривой способ удаления всех внешних скобок
        while (removeOuterBrackets(str).also { str = it } !== removeOuterBrackets(str));
        //конец рекурсии, если блок содержит только число
        if (isNumber(str)) {
            return str.toDouble()
        }
        println(str)
        //вызываем метод, который делит строку на два блока, делает  вызов expression() внутри и так пока не наткнемся на блок, состоящий только из числа
        return splicing(str)
    }

    private fun splicing(str: String): Double {
        //ищем точку для разделения строки на два блока
        val splice = findSplice(str)
        val block1: String = str.substring(0, splice)
        val block2: String = str.substring(splice + 1)
        println(block1 + " [" + str[splice] + "] " + block2)
        when (str[splice]) {
            '+' -> return calculate(block1) + calculate(block2)
            '-' -> return calculate(block1) - calculate(block2)
            '*' -> return calculate(block1) * calculate(block2)
            '/' -> return calculate(block1) / calculate(block2)
        }
        return 0.0 //никогда сюда не попадем
    }

    private fun removeOuterBrackets(str: String): String {
        if (str.startsWith("(") && str.endsWith(")")) {
            var count = 0
            for (i in 1 until str.length - 1) {
                when (str[i]) {
                    '(' -> count++
                    ')' -> count--
                }
                if (count < 0) {
                    return str //возвращаем исходную строку, т.к. нечго удалять
                }
            }
            require(count == 0) { "Brakets! (()" }
            return str.substring(1, str.length - 1) //возвращаем строку без одних внешних скобок
        }
        return str //никогда сюда не попадем
    }

    private fun isNumber(str: String): Boolean {
        try {
            str.toDouble()
        } catch (e: NumberFormatException) {
            return false
        }
        return true
    }

    //приоритет при поиске разделителя у + -, потом * /
    // (5+6)*7+8  -> (5+6)*7 [+] 8
    // если не можем найти + или - для разделения строки, ищем * /
    // если не находим, то кидается ошибка, т.к. сюда строка только с числом не может попасть
    private fun findSplice(str: String): Int {
        var count = 0
        for (i in str.indices) {
            when (str[i]) {
                '(' -> count++
                ')' -> count--
            }
            require(count >= 0) { "Brackets! )(" }
            if (count == 0 &&
                (str[i] == '+' || str[i] == '-')
            ) {
                return i
            }
        }
        count = 0
        for (i in str.indices) {
            when (str[i]) {
                '(' -> count++
                ')' -> count--
            }
            require(count >= 0) { "Brackets! )(" }
            if (count == 0 &&
                (str[i] == '*' || str[i] == '/')
            ) {
                //TODO вызов метода для конвертации строки 1/7/5*2 -> 1*2/(7*5)
                //а лучше вообще переписать findSplice и removeOuterBrackets, много повторяющегося кода
                return i
            }
        }
        require(count == 0) { "Brakets! (()" }
        require(count != 0) { "symbols||spaces between numbers||double operators|| *) /) (+ etc" }
        return 0 //никогда сюда не попадем
    }

    fun convert(str: String?): Int {
        //TODO конвертация строки 1/7/5*2 -> 1*2/(7*5)
        return 0
    }

    @JvmStatic
    fun main(args: Array<String>) {

    }
}
