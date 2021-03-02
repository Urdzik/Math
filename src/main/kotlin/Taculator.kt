import TaculatorExtension.have
import cern.jet.random.Binomial
import cern.jet.random.Normal
import cern.jet.random.Poisson
import cern.jet.random.engine.RandomEngine
import org.apache.commons.math3.analysis.differentiation.DerivativeStructure
import org.apache.commons.math3.special.Erf
import org.apache.commons.math3.stat.descriptive.moment.StandardDeviation
import org.apache.commons.math3.stat.descriptive.moment.Variance
import org.mariuszgromada.math.mxparser.Argument
import org.mariuszgromada.math.mxparser.Expression
import umontreal.ssj.probdist.BinomialDist
import umontreal.ssj.probdist.GeometricDist
import umontreal.ssj.util.Num
import kotlin.math.pow
import java.text.DecimalFormat
import kotlin.random.Random

object Taculator {

    fun plus(a: Number, b: Number): Number {
        val a_data: Double = a.toDouble()
        val b_data: Double = b.toDouble()
        return a_data + b_data
    }

    fun minus(a: Number, b: Number): Number {
        val a_data: Double = a.toDouble()
        val b_data: Double = b.toDouble()
        return a_data - b_data
    }

    fun multiply(a: Number, b: Number): Number {
        val a_data: Double = a.toDouble()
        val b_data: Double = b.toDouble()
        return a_data * b_data
    }

    fun share(a: Number, b: Number): Number {
        val a_data: Double = a.toDouble()
        val b_data: Double = b.toDouble()
        return a_data / b_data
    }

    fun sqrt(a: Number, b: Number = 2.0): Number {
        val a_data: Double = a.toDouble()
        val b_data: Double = b.toDouble()
        return a_data.pow(1 / b_data)
    }

    fun degree(a: Number, b: Number = 2.0): Number {
        val a_data: Double = a.toDouble()
        val b_data: Double = b.toDouble()
        return a_data.pow(b_data)
    }

    fun frac(a: Number): String {
        val a_data: Double = a.toDouble()
        if (a_data < 0) {
            return "-" + frac(-a_data)
        }
        val tolerance = 1.0E-6
        var h1 = 1.0
        var h2 = 0.0
        var k1 = 0.0
        var k2 = 1.0
        var b = a_data
        do {
            val x = Math.floor(b)
            var aux = h1
            h1 = x * h1 + h2
            h2 = aux
            aux = k1
            k1 = x * k1 + k2
            k2 = aux
            b = 1 / (b - x)
        } while (Math.abs(a_data - h1 / k1) > a_data * tolerance)
        return "${h1.toInt()}/${k1.toInt()}"
    }

    fun dec(s: String): Number {
        val a_data = s.split("/")[0].toDouble()
        val b_data = s.split("/")[1].toDouble()
        return a_data / b_data
    }

    fun abs(a: Number): Number {
        return kotlin.math.abs(a.toDouble())
    }

    fun round(a: Number, b: Int): Number {
        val a_data: Double = a.toDouble()
        val b_data: Int = b
        val df: DecimalFormat
        var dfPattern = "#"
        if (b_data == 0) {
            df = DecimalFormat(dfPattern)
        } else {
            dfPattern = "#."
            for (i in 1..b_data) {
                dfPattern += "#"

            }
            df = DecimalFormat(dfPattern)
        }

        return df.format(a_data).replace(',', '.').toDouble()
    }

    fun iPart(a: Number): String {
        val s = a.toString()
        val a_data = s.split(".")[0]
        return a_data
    }

    fun fPart(a: Number): String {
        val s = a.toString()
        val a_data = s.split(".")[1]
        return ".$a_data"
    }

    fun int(a: Number): Number {
        val a_data: Double = a.toDouble()
        return Math.ceil(a_data).toInt()
    }

    fun min(a: Number, b: Number): Number {
        val a_data: Double = a.toDouble()
        val b_data: Double = b.toDouble()
        return Math.min(a_data, b_data)
    }

    fun min(a: List<Number>, b: Number): Number {
        val a_data: List<Double> = a.map { it.toDouble() }
        val b_data: Double = b.toDouble()
        val a_min = a_data.minOrNull()
        // FIXME fix error if a_min == null
        return Math.min(a_min!!, b_data)
    }

    fun min(a: List<Number>): Number {
        val a_data: Double? = a.map { it.toDouble() }.minOrNull()
        // FIXME fix error if a_data == null
        return a_data!!
    }

    fun max(a: Number, b: Number): Number {
        val a_data: Double = a.toDouble()
        val b_data: Double = b.toDouble()
        return Math.max(a_data, b_data)
    }

    fun max(a: List<Number>, b: Number): Number {
        val a_data: List<Double> = a.map { it.toDouble() }
        val b_data: Double = b.toDouble()
        val a_max = a_data.maxOrNull()
        // FIXME fix error if a_min == null
        return Math.max(a_max!!, b_data)
    }

    fun max(a: List<Number>): Number {
        val a_data: Double? = a.map { it.toDouble() }.maxOrNull()
        // FIXME fix error if a_data == null
        return a_data!!
    }

    fun lsm(a: Number, b: Number): Number {
        val a_data: Double = a.toDouble()
        val b_data: Double = b.toDouble()
        if (a_data == 0.0 || b_data == 0.0) {
            return 0
        }
        val absNumber1 = Math.abs(a_data)
        val absNumber2 = Math.abs(b_data)
        val absHigherNumber = Math.max(absNumber1, absNumber2)
        val absLowerNumber = Math.min(absNumber1, absNumber2)
        var lcm = absHigherNumber
        while (lcm % absLowerNumber != 0.0) {
            lcm += absHigherNumber
        }
        return lcm
    }

    fun gcd(a: Number, b: Number): Number {
        val a_data: Double = a.toDouble()
        val b_data: Double = b.toDouble()
        var gcd = 1.0
        var i = 1.0
        while (i <= a_data && i <= b_data) {
            if (a_data % i == 0.0 && b_data % i == 0.0) {
                gcd = i
            }
            i++
        }
        return gcd
    }

    fun remainder(a: Number, b: Number): Number {
        val a_data: Double = a.toDouble()
        val b_data: Double = b.toDouble()
        return a_data % b_data
    }

    fun conj(s: String): String {
        val listOfAllNumber = mutableListOf<String>()
        val listOfComplexNumber = mutableListOf<String>()
        val listOfSimpleNumber = mutableListOf<String>()

        var simple = 0.0
        var simpleExample = ""
        var simpleResult = ""
        var complex = 0.0
        var complexExample = ""
        var complexResult = ""
        var example = ""
        var numberInStr = ""
        var result = ""

        s.forEachIndexed { index, c ->
            if (index != s.length - 1) {
                if (numberInStr.isNotEmpty() && (c != '-' || c != '+')) {
                    if (c != '-' && c != '+' && c != '*' && c != '/') {
                        numberInStr += c
                    } else {
                        if (numberInStr.isNotEmpty()) {
                            listOfAllNumber.add(numberInStr)
                        }
                        if (c != '*' || c != '/') {
                            numberInStr = c.toString()
                        }
                    }
                } else {
                    numberInStr += c
                }
            } else {
                numberInStr += c
                listOfAllNumber.add(numberInStr)
                numberInStr = ""
            }
        }

        listOfAllNumber.forEach {
            val data = it.find { it == 'i' }
            if (data == null) {
                listOfSimpleNumber.add(it)
            } else {
                listOfComplexNumber.add(it)
            }
        }

        listOfComplexNumber.forEach {
            val data = it.removeSuffix("i")
            complexExample += data
        }

        listOfSimpleNumber.forEach {
            simpleExample += it
        }

        if (complexExample[0] == '+' || complexExample[0] == '/' || complexExample[0] == '*') {
            example = complexExample[0].toString()
            val data = complexExample.removePrefix(complexExample[0].toString())
            complexExample = data
        } else if (complexExample[0] == '-') {
            complexExample = ""
            listOfComplexNumber.forEachIndexed { index, s ->
                var data = s.removeSuffix("i")
                if (index == 0) {
                    data = "($data)"
                }
                complexExample += data
            }

        }

        if (simpleExample[0] == '+' || simpleExample[0] == '/' || simpleExample[0] == '*') {
            val data = simpleExample.removePrefix(simpleExample[0].toString())
            simpleExample = data
        }

        complex = DynamicCalculator.calculate(complexExample)
        simple = DynamicCalculator.calculate(simpleExample)

        complexResult = if (complex > 0) {
            "-$complex"
        } else {
            "$complex"
        }

        simpleResult = "$simple"

        result = if (example == "*" || example == "/") {
            DynamicCalculator.calculate("($simpleResult)$example($complexResult)").toString() + "i"
        } else {
            simpleResult + complexResult + "i"
        }


        return result
    }

    fun real(s: String): String {
        val listOfAllNumber = mutableListOf<String>()
        val listOfSimpleNumber = mutableListOf<String>()

        var simple = 0.0
        var simpleExample = ""
        var simpleResult = ""
        var numberInStr = ""
        var result = ""

        s.forEachIndexed { index, c ->
            if (index != s.length - 1) {
                if (numberInStr.isNotEmpty() && (c != '-' || c != '+')) {
                    if (c != '-' && c != '+' && c != '*' && c != '/') {
                        numberInStr += c
                    } else {
                        if (numberInStr.isNotEmpty()) {
                            listOfAllNumber.add(numberInStr)
                        }
                        if (c != '*' || c != '/') {
                            numberInStr = c.toString()
                        }
                    }
                } else {
                    numberInStr += c
                }
            } else {
                numberInStr += c
                listOfAllNumber.add(numberInStr)
                numberInStr = ""
            }
        }
        listOfAllNumber.forEach { if (it.find { it == 'i' } == null) listOfSimpleNumber.add(it) }
        listOfSimpleNumber.forEach { simpleExample += it }

        if (simpleExample[0] == '+' || simpleExample[0] == '/' || simpleExample[0] == '*') {
            val data = simpleExample.removePrefix(simpleExample[0].toString())
            simpleExample = data
        }

        simple = DynamicCalculator.calculate(simpleExample)

        simpleResult = "$simple"

        result = DynamicCalculator.calculate(simpleResult).toString()

        return result
    }

    fun imag(s: String): String {
        val listOfAllNumber = mutableListOf<String>()
        val listOfComplexNumber = mutableListOf<String>()


        var complex = 0.0
        var complexExample = ""
        var complexResult = ""
        var numberInStr = ""
        var result = ""

        s.forEachIndexed { index, c ->
            if (index != s.length - 1) {
                if (numberInStr.isNotEmpty() && (c != '-' || c != '+')) {
                    if (c != '-' && c != '+' && c != '*' && c != '/') {
                        numberInStr += c
                    } else {
                        if (numberInStr.isNotEmpty()) {
                            listOfAllNumber.add(numberInStr)
                        }
                        if (c != '*' || c != '/') {
                            numberInStr = c.toString()
                        }
                    }
                } else {
                    numberInStr += c
                }
            } else {
                numberInStr += c
                listOfAllNumber.add(numberInStr)
                numberInStr = ""
            }
        }

        listOfAllNumber.forEach { if (it.find { it == 'i' } != null) listOfComplexNumber.add(it) }

        listOfComplexNumber.forEach {
            val data = it.removeSuffix("i")
            complexExample += data
        }



        if (complexExample[0] == '+' || complexExample[0] == '/' || complexExample[0] == '*') {
            val data = complexExample.removePrefix(complexExample[0].toString())
            complexExample = data
        } else if (complexExample[0] == '-') {
            complexExample = ""
            listOfComplexNumber.forEachIndexed { index, s ->
                var data = s.removeSuffix("i")
                if (index == 0) {
                    data = "($data)"
                }
                complexExample += data
            }

        }


        complex = DynamicCalculator.calculate(complexExample)

        complexResult = "$complex"



        result = DynamicCalculator.calculate("$complexResult").toString() + "i"


        return result
    }

    fun angle(s: String): Number {
        val listOfAllNumber = mutableListOf<String>()
        val listOfComplexNumber = mutableListOf<String>()
        val listOfSimpleNumber = mutableListOf<String>()

        var simple = 0.0
        var simpleExample = ""
        var complex = 0.0
        var complexExample = ""
        var numberInStr = ""
        var result = 0.0

        s.forEachIndexed { index, c ->
            if (index != s.length - 1) {
                if (numberInStr.isNotEmpty() && (c != '-' || c != '+')) {
                    if (c != '-' && c != '+' && c != '*' && c != '/') {
                        numberInStr += c
                    } else {
                        if (numberInStr.isNotEmpty()) {
                            listOfAllNumber.add(numberInStr)
                        }
                        if (c != '*' || c != '/') {
                            numberInStr = c.toString()
                        }
                    }
                } else {
                    numberInStr += c
                }
            } else {
                numberInStr += c
                listOfAllNumber.add(numberInStr)
                numberInStr = ""
            }
        }

        listOfAllNumber.forEach {
            val data = it.find { it == 'i' }
            if (data == null) {
                listOfSimpleNumber.add(it)
            } else {
                listOfComplexNumber.add(it)
            }
        }

        listOfComplexNumber.forEach {
            val data = it.removeSuffix("i")
            complexExample += data
        }

        listOfSimpleNumber.forEach {
            simpleExample += it
        }

        if (complexExample[0] == '+' || complexExample[0] == '/' || complexExample[0] == '*') {
            val data = complexExample.removePrefix(complexExample[0].toString())
            complexExample = data
        } else if (complexExample[0] == '-') {
            complexExample = ""
            listOfComplexNumber.forEachIndexed { index, s ->
                var data = s.removeSuffix("i")
                if (index == 0) {
                    data = "($data)"
                }
                complexExample += data
            }

        }

        if (simpleExample[0] == '+' || simpleExample[0] == '/' || simpleExample[0] == '*') {
            val data = simpleExample.removePrefix(simpleExample[0].toString())
            simpleExample = data
        }

        complex = DynamicCalculator.calculate(complexExample)
        simple = DynamicCalculator.calculate(simpleExample)


        result = Math.atan2(complex, simple)

        return result
    }

    fun angle(a: Number): Number {
        val result = Math.atan(a.toDouble())
        return result
    }

    fun abs(s: String): Number {
        val listOfAllNumber = mutableListOf<String>()
        val listOfComplexNumber = mutableListOf<String>()
        val listOfSimpleNumber = mutableListOf<String>()

        var simple = 0.0
        var simpleExample = ""
        var complex = 0.0
        var complexExample = ""
        var numberInStr = ""
        var result = 0.0

        s.forEachIndexed { index, c ->
            if (index != s.length - 1) {
                if (numberInStr.isNotEmpty() && (c != '-' || c != '+')) {
                    if (c != '-' && c != '+' && c != '*' && c != '/') {
                        numberInStr += c
                    } else {
                        if (numberInStr.isNotEmpty()) {
                            listOfAllNumber.add(numberInStr)
                        }
                        if (c != '*' || c != '/') {
                            numberInStr = c.toString()
                        }
                    }
                } else {
                    numberInStr += c
                }
            } else {
                numberInStr += c
                listOfAllNumber.add(numberInStr)
                numberInStr = ""
            }
        }

        listOfAllNumber.forEach {
            val data = it.find { it == 'i' }
            if (data == null) {
                listOfSimpleNumber.add(it)
            } else {
                listOfComplexNumber.add(it)
            }
        }

        listOfComplexNumber.forEach {
            val data = it.removeSuffix("i")
            complexExample += data
        }

        listOfSimpleNumber.forEach {
            simpleExample += it
        }

        if (complexExample[0] == '+' || complexExample[0] == '/' || complexExample[0] == '*') {
            val data = complexExample.removePrefix(complexExample[0].toString())
            complexExample = data
        } else if (complexExample[0] == '-') {
            complexExample = ""
            listOfComplexNumber.forEachIndexed { index, s ->
                var data = s.removeSuffix("i")
                if (index == 0) {
                    data = "($data)"
                }
                complexExample += data
            }

        }

        if (simpleExample[0] == '+' || simpleExample[0] == '/' || simpleExample[0] == '*') {
            val data = simpleExample.removePrefix(simpleExample[0].toString())
            simpleExample = data
        }

        complex = DynamicCalculator.calculate(complexExample)
        simple = DynamicCalculator.calculate(simpleExample)


        result = sqrt(degree(simple).toDouble() + degree(complex).toDouble()).toDouble()

        return result
    }

    fun rand(a: Long = 1): String {
        val list = mutableListOf<Double>()
        for (i in 0 until (a.toInt())) {
            list.add(Random.nextDouble(0.0, 1.0))
        }
        return list.joinToString { it.toString() }
    }

    fun nPr(a: Number, b: Number): Number {
        val a_data: Double = a.toDouble()
        val b_data: Double = b.toDouble()
        var result = 1.0
        for (i in 0 until b_data.toInt()) {
            result *= a_data - i
        }
        return result
    }

    fun nCr(a: Number, b: Number): Number {
        val a_data: Double = a.toDouble()
        val b_data: Double = b.toDouble()
        var result = 1
        result = factorial(a_data) / (factorial(b_data) * factorial(a_data - b_data));
        return result
    }

    fun factorial(a: Number): Int {
        val a_data: Double = a.toDouble()
        var fact = 1
        var i = 1
        while (i <= a_data) {
            fact *= i
            i++
        }
        return fact
    }

    fun randInt(a: Int, b: Int, c: Int = 1): String {
        val list = mutableListOf<Int>()
        for (i in 0 until c) {
            val result = Random.nextInt(a, b)
            list.add(result)
        }
        return list.joinToString { it.toString() }
    }

    fun randNorm(a: Number, b: Number): Number {
        val a_data: Double = a.toDouble()
        val b_data: Double = b.toDouble()
        val result = Normal.staticNextDouble(a_data, b_data)
        return result
    }

    // TODO I think this function work right, but i not sure
    fun randBin(a: Number, b: Number, count: Int = 1): String {
        val n: Int = a.toInt()
        val p: Double = b.toDouble()
        val listOfBinomial = mutableListOf<Number>()
        val binom = Binomial(n, p, RandomEngine.makeDefault())
        for (i in 0 until count) {
            listOfBinomial.add(binom.nextDouble())
        }
        return listOfBinomial.joinToString { it.toString() }
    }


    fun randIntNoRep(a: Int, b: Int, c: Int = 1): String {
        val list = mutableListOf<Int>()
        var i = 1
        while (i <= c) {
            val result = Random.nextInt(a, b)
            val data = list.find { it == result }
            if (data == null) {
                list.add(result)
                i++
            }
        }
        return list.joinToString { it.toString() }
    }


    fun equally(a: Number, b: Number): Int {
        val a_data: Double = a.toDouble()
        val b_data: Double = b.toDouble()
        val bool = (a_data == b_data)
        return if (bool) 1 else 0
    }

    fun notEqually(a: Number, b: Number): Int {
        val a_data: Double = a.toDouble()
        val b_data: Double = b.toDouble()
        val bool = (a_data != b_data)
        return if (bool) 1 else 0
    }

    fun more(a: Number, b: Number): Int {
        val a_data: Double = a.toDouble()
        val b_data: Double = b.toDouble()
        val bool = (a_data > b_data)
        return if (bool) 1 else 0
    }

    fun moreOrEqually(a: Number, b: Number): Int {
        val a_data: Double = a.toDouble()
        val b_data: Double = b.toDouble()
        val bool = (a_data >= b_data)
        return if (bool) 1 else 0
    }

    fun less(a: Number, b: Number): Int {
        val a_data: Double = a.toDouble()
        val b_data: Double = b.toDouble()
        val bool = (a_data < b_data)
        return if (bool) 1 else 0
    }

    fun lessOrEqually(a: Number, b: Number): Int {
        val a_data: Double = a.toDouble()
        val b_data: Double = b.toDouble()
        val bool = (a_data <= b_data)
        return if (bool) 1 else 0
    }

    fun and(a: Int, b: Int): Int {
        val a_data: Boolean = a == 1
        val b_data: Boolean = b == 1
        val bool = (a_data && b_data)
        return if (bool) 1 else 0
    }

    fun or(a: Number, b: Number): Int {
        val a_data: Boolean = a == 1
        val b_data: Boolean = b == 1
        val bool = (a_data || b_data)
        return if (bool) 1 else 0
    }

    fun xor(a: Number, b: Number): Int {
        val a_data: Boolean = a == 1
        val b_data: Boolean = b == 1
        val bool = (a_data == b_data)
        return if (bool) 1 else 0
    }

    fun not(a: Number): Int {
        val a_data: Boolean = a == 1
        val bool = !a_data
        return if (bool) 1 else 0
    }

    fun degreesToRadians(a: Number): Number {
        val a_data: Double = a.toDouble()
        return Math.toRadians(a_data)
    }

    fun radiansToDegrees(a: Number): Number {
        val a_data: Double = a.toDouble()
        return Math.toDegrees(a_data)
    }

    fun degreesToRadians(degrees: Number, minutes: Number, seconds: Number): Number {
        val degrees_data: Double = degrees.toDouble()
        val minutes_data: Double = minutes.toDouble()
        val seconds_data: Double = seconds.toDouble()
        val result = degrees_data + minutes_data / 60.0 + seconds_data / 3600.0
        return Math.toRadians(result)
    }

    fun det(matrix: Array<DoubleArray>): Number {
        val result = MatrixOperations.matrixDeterminant(matrix)
        return result
    }

    fun transpose(matrix: Array<DoubleArray>): String {
        val result = MatrixOperations.transposeMatrix(matrix)
        MatrixOperations.printMatrix(result, 4)
        return result.joinToString { it.joinToString { it.toString() } }
    }

    fun dim(matrix: Array<DoubleArray>): Int {
        var result = 0
        matrix.forEach {
            result += it.size
        }
        return result
    }

    fun stoDim(size: Int): Array<DoubleArray> {
        return arrayOf(DoubleArray(size))
    }

    fun fill(number: Number, matrix: Array<DoubleArray>): Array<DoubleArray> {
        val matrix = matrix
        matrix.forEachIndexed { index, doubles ->
            for ((i, data) in doubles.withIndex()) {
                doubles[i] = number.toDouble()
            }
        }
        return matrix
    }

    // FIXME: 2/25/21 I don't know, how to do it
    fun fill(matrix: Array<DoubleArray>): String {
        return matrix.apply { this.fill(this[0], 0, 9) }.joinToString { it.joinToString { it.toString() } }
    }

    fun identity(countOfRowAndCollum: Int): Array<IntArray> {
        var cinema = arrayOf<IntArray>()

        for (i in 0 until countOfRowAndCollum) {
            var array = arrayOf<Int>()
            for (j in 0 until countOfRowAndCollum) {
                array += 0
            }
            cinema += array.toIntArray()
        }
        MatrixOperations.printMatrix(cinema, 5)
        return cinema
    }

    fun randM(countOfRow: Int, countOfCollum: Int): Array<IntArray> {
        var cinema = arrayOf<IntArray>()

        for (i in 0 until countOfRow) {
            var array = arrayOf<Int>()
            for (j in 0 until countOfCollum) {
                array += Random.nextInt(-9, 9)
            }
            cinema += array.toIntArray()
        }
        MatrixOperations.printMatrix(cinema, 5)
        return cinema
    }


    fun cumSum(a: IntArray): IntArray {
        val out = IntArray(a.size)
        var total = 0
        for (i in a.indices) {
            total += a[i]
            out[i] = total
        }
        return out
    }

    fun cumSum(a: Array<IntArray>): Array<IntArray> {
        val out = IntArray(a.size)
        var total = 0
        val list = mutableListOf<IntArray>()
        a.forEach {
            for (i in it.indices) {
                total += it[i]
                out[i] = total
            }
            list.add(it)
        }

        return list.toTypedArray()
    }

    fun rref(matrix: Array<DoubleArray>): Array<DoubleArray>? {
        val result = MatrixOperations.rref(matrix)
        MatrixOperations.printMatrix(result, 5)
        return result
    }

    fun ref(matrix: Array<DoubleArray>): Array<DoubleArray>? {
        val result = MatrixOperations.ref(matrix)
        MatrixOperations.printMatrix(result, 5)
        return result
    }

    fun normalpdf(a: Number, b: Number, c: Number): Number {
        val normal = Normal(b.toDouble(), c.toDouble(), null)
        val result = normal.pdf(a.toDouble())
        return result
    }

    fun normalcdf(lower: Number, upper: Number): Number {
        val a = (lower.toDouble()) / (kotlin.math.sqrt(2.0))
        val b = (upper.toDouble()) / (kotlin.math.sqrt(2.0))
        val result = 0.5 * ((Erf.erf(b)) - (Erf.erf(a)))
        return result
    }

    fun normalcdf(lower: Number, upper: Number, u: Number, o: Number): Number {
        val a = (lower.toDouble() - u.toDouble()) / o.toDouble()
        val b = (upper.toDouble() - u.toDouble()) / o.toDouble()
        val result = normalcdf(a, b)
        return result
    }

    fun invNorm(a: Number): Number {
        return 2 / Math.sqrt(2.0) * Erf.erfInv(2 * a.toDouble() - 1)
    }

    fun invNorm(a: Number, u: Number, o: Number): Number {
        return u.toDouble() + o.toDouble() * invNorm(a.toDouble()).toDouble()
    }

    fun invT(a: Number, b: Number, c: Number): Number {
        val binomial = BinomialDist.inverseF(a.toInt(), b.toDouble(), c.toDouble())
        return binomial
    }

    fun poissonpdf(a: Number, b: Number): Number {
        val poisson = Poisson(a.toDouble(), null)
        return poisson.pdf(b.toInt())
    }

    fun poissoncdf(a: Number, b: Number): Number {
        val poisson = Poisson(a.toDouble(), null)
        return poisson.cdf(b.toInt())
    }

    fun binompdf(a: Number, b: Number, c: Number): Number {
        val binom = Binomial(a.toInt(), b.toDouble(), null)
        val result = binom.pdf(c.toInt())
        return result
    }

    fun geometpdf(a: Number, b: Number): Number {
        val poisson = GeometricDist(a.toDouble())
        return poisson.barF(b.toInt())
    }

    fun geometcdf(p: Number, n: Number): Number {
        val result = 1 - (1 - p.toDouble()).pow(n.toDouble())
        return result
    }

    fun SortA(array: Array<Number>): Array<Number> {
        return array.apply { sort() }
    }

    fun SortD(array: Array<Number>): Array<Number> {
        return array.apply { sort() }.reversedArray()
    }

    fun mean(array: Array<Number>): Number {
        var result = 0.0
        if (array.isNotEmpty()) {
            for (mark in array) {
                result += mark.toDouble()
            }
            return result / array.size
        }
        return result
    }

    fun mean(array1: Array<Number>, array2: Array<Number>): Number {
        val resultList = mutableListOf<Number>()
        array1.forEachIndexed { index, number ->
            for (item in 0 until array2[index].toInt()) {
                resultList.add(number)
            }
        }


        val result = mean(resultList.toTypedArray())
        return result
    }

    fun median(array: Array<Number>): Number {
        return if (array.size % 2 == 0) {
            (array[array.size / 2].toDouble() + array[array.size / 2 - 1].toDouble()) / 2
        } else {
            array[array.size / 2]
        }
    }

    fun median(array1: Array<Number>, array2: Array<Number>): Number {
        val resultList = mutableListOf<Number>()
        array1.forEachIndexed { index, number ->
            for (item in 0 until array2[index].toInt()) {
                resultList.add(number)
            }
        }

        val result = median(resultList.toTypedArray())
        return result
    }

    fun sum(array: Array<Number>): Number {
        var result = 0.0
        array.forEach {
            result += it.toDouble()
        }
        return result
    }

    fun sum(array: Array<Number>, lower: Int, upper: Int): Number {
        var result = 0.0
        array.forEachIndexed { index: Int, number: Number ->
            if (index in lower..upper) {
                result += number.toDouble()
            }
        }
        return result
    }

    fun prod(array: Array<Number>): Number {
        var result = 1.0
        array.forEach {
            result *= it.toDouble()
        }
        return result
    }

    fun prod(array: Array<Number>, lower: Int, upper: Int): Number {
        var result = 1.0
        array.forEachIndexed { index: Int, number: Number ->
            if (index in lower..upper) {
                result *= number.toDouble()
            }
        }
        return result
    }

    fun stdDev(array: Array<Number>): Number {
        val arr = array.map { it.toDouble() }.toDoubleArray()
        val result = StandardDeviation().evaluate(arr)
        return result
    }

    fun stdDev(array1: Array<Number>, array2: Array<Number>): Number {
        val resultList = mutableListOf<Number>()
        array1.forEachIndexed { index, number ->
            for (item in 0 until array2[index].toInt()) {
                resultList.add(number)
            }
        }


        val result = stdDev(resultList.toTypedArray())
        return result
    }

    fun variance(array: Array<Number>): Number {
        val arr = array.map { it.toDouble() }.toDoubleArray()
        val result = Variance().evaluate(arr)
        return result
    }

    fun variance(array1: Array<Number>, array2: Array<Number>): Number {
        val resultList = mutableListOf<Number>()
        array1.forEachIndexed { index, number ->
            for (item in 0 until array2[index].toInt()) {
                resultList.add(number)
            }
        }
        val result = variance(resultList.toTypedArray())
        return result
    }


    fun nDeriv(func: String, x: Number): Number {
        val result = Expression("der($func, x, $x)")
        return Math.ceil(result.calculate())
    }

    fun fInt(func: String, from: Number, to: Number): Number {
        val result = Expression("int($func, x, $from, $to)")
        return Math.ceil(result.calculate())
    }

    fun summation(func: String, min: Number, max: Number): Number {
        val result = Expression("sum(x, $min, $max, $func)")
        return Math.ceil(result.calculate())
    }

    fun logBASE(a: Number, b: Number): Number {
        val a_data: Double = a.toDouble()
        val b_data: Double = b.toDouble()
        val result = Math.ceil(a_data.pow(1 / b_data))
        return result
    }


//    FIXME: 3/2/21 i don't know, what doing these function
//    fun fMin(func: String, min: Number, max: Number): Number{
//        val result = Expression("mini(x, $min, $max, $func)")
//        return result.calculate()
//    }
//
//    fun fMax(func: String, min: Number, max: Number): Number {
//        val result = Expression("maxi(x, $min, $max, $func)")
//        return result.calculate()
//    }

    fun fd(a: Number): String {
        val a_data: Double = a.toDouble()
        if (a_data < 0) {
            return "-" + frac(-a_data)
        }
        val tolerance = 1.0E-6
        var h1 = 1.0
        var h2 = 0.0
        var k1 = 0.0
        var k2 = 1.0
        var b = a_data
        do {
            val x = Math.floor(b)
            var aux = h1
            h1 = x * h1 + h2
            h2 = aux
            aux = k1
            k1 = x * k1 + k2
            k2 = aux
            b = 1 / (b - x)
        } while (Math.abs(a_data - h1 / k1) > a_data * tolerance)
        return "${h1.toInt()}/${k1.toInt()}"
    }

    fun ndToUnd(s: String): String {
        val listOfNumber = s.split('/')
        val a_data = listOfNumber[0].toDouble()
        val b_data = listOfNumber[1].toDouble()
        val u: Int
        val n: Int
        val d: Int = b_data.toInt()
        u = (a_data / b_data).toInt()
        n = (a_data - (u * b_data)).toInt()
        return "$u $n/$d"
    }

    fun ndToUnd(u: Number, s: String): String {
        val listOfNumber = s.split('/')
        val a_data = listOfNumber[0].toInt()
        val b_data = listOfNumber[1].toInt()
        val u_data: Int
        var n: Int = a_data.toInt()
        val d: Int = b_data.toInt()
        u_data = (a_data / b_data).toInt() + u.toInt()
        n += (u_data * b_data).toInt()
        return "$n/$d"
    }

    //TODO i don't understand, what doing these functions
    fun rect() {

    }

    fun polar() {

    }

    fun radian(func: String): Number {
        val result = Expression("deg($func)").calculate()
        return result
    }

    fun dns(data: String): String {

        var result = ""
        var minutes = 0.0
        var seconds = 0.0
        var milliSeconds = 0.0

        val m = '°'
        val dot = '.'
        val s = '\''
        val ms = '\"'

        if (data.have(m)) {
            val minAndOther = data.split(m)
            minutes = minAndOther[0].toDouble()
            if (data.have(s)) {
                val secondsAndMilliSeconds = minAndOther[1].split(s)
                seconds = secondsAndMilliSeconds[0].toDouble()
                if (data.have(ms)) {
                    milliSeconds = secondsAndMilliSeconds[1].removeSuffix("\"").toDouble()
                }
            }
        }

        if (milliSeconds >= 60) {
            val milliSecondsInt = milliSeconds.toInt()
            val remainder = milliSecondsInt % 60
            milliSeconds = if (milliSeconds.toString().have(dot)) {
                val afterDot = milliSeconds.toString().split(dot)[1]
                "$remainder.$afterDot".toDouble()
            } else {
                remainder.toDouble()
            }
            seconds += milliSecondsInt / 60
        }

        if (seconds >= 60) {
            val secondsInt = seconds.toInt()
            val remainder = secondsInt % 60
            seconds = if (seconds.toString().have(dot)) {
                var afterDot = seconds.toString().split(dot)[1]
               "$remainder.$afterDot".toDouble()
            } else {
                remainder.toDouble()
            }
            minutes += secondsInt / 60
        }

        if (seconds.toString().have(dot)){
            val secondsInt = seconds.toInt()
            val remainder = secondsInt % 60
            var afterDot = seconds.toString().split(dot)[1]
            if (afterDot.toDouble() <= 9){
                afterDot = (afterDot.toDouble()*10).toString()
            }
          seconds =  if (afterDot.toDouble() >= 60){
                val afterDotInt = afterDot.toDouble().toInt()
                val afterDotRemainder = afterDotInt % 60
                milliSeconds += afterDotInt / 60
                "$remainder.$afterDotRemainder".toDouble()
            }else {
                seconds
            }
        }

        if (milliSeconds >= 60) {
            val milliSecondsInt = milliSeconds.toInt()
            val remainder = milliSecondsInt % 60
            milliSeconds = if (milliSeconds.toString().have(dot)) {
                val afterDot = milliSeconds.toString().split(dot)[1]
                "$remainder.$afterDot".toDouble()
            } else {
                remainder.toDouble()
            }
            seconds += milliSecondsInt / 60
        }

        if (minutes.toString().have(dot)){
            val minutesInt = minutes.toInt()
            val remainder = minutesInt % 60
            var afterDot = minutes.toString().split(dot)[1]
            if (afterDot.toDouble() <= 9){
                afterDot = (afterDot.toDouble()*10).toString()
            }
            minutes =  if (afterDot.toDouble() >= 60){
                val afterDotInt = afterDot.toDouble().toInt()
                val afterDotRemainder = afterDotInt % 60
                seconds += afterDotInt / 60
                "$remainder.$afterDotRemainder".toDouble()
            }else {
                minutes
            }
        }

        if (seconds >= 60) {
            val secondsInt = seconds.toInt()
            val remainder = secondsInt % 60
            seconds = if (seconds.toString().have(dot)) {
                var afterDot = seconds.toString().split(dot)[1]
                "$remainder.$afterDot".toDouble()
            } else {
                remainder.toDouble()
            }
            minutes += secondsInt / 60
        }

        result = "$minutes°$seconds'$milliSeconds\""
        return result
    }


}



