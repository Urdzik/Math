import cern.jet.random.Normal
import cern.jet.random.Poisson
import org.apache.commons.math3.distribution.BinomialDistribution
import org.apache.commons.math3.distribution.GeometricDistribution
import org.apache.commons.math3.distribution.PoissonDistribution
import org.apache.commons.math3.random.RandomGenerator
import org.apache.commons.math3.special.Erf
import umontreal.ssj.probdist.BinomialDist
import umontreal.ssj.probdist.GeometricDist
import umontreal.ssj.probdist.PoissonDist
import umontreal.ssj.util.Num
import kotlin.math.pow
import java.text.DecimalFormat
import kotlin.random.Random
import kotlin.random.Random.Default.nextInt

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

    // TODO Add those func
    fun randNorm(a: Int, b: Int): Number {
        val a_data: Double = a.toDouble()
        val b_data: Double = b.toDouble()

        var result = 0.0

        result = java.util.Random().nextGaussian()


        return result
    }

    fun randBin(a: Int, b: Int): Number {
        val a_data: Double = a.toDouble()
        val b_data: Double = b.toDouble()

        return 0.0
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

    // TODO add dim with sto→
    fun dim(matrix: Array<DoubleArray>): Int {
        var result = 0
        matrix.forEach {
            result += it.size
        }
        return result
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
        val binomial = BinomialDist.inverseF(a.toInt(), b.toDouble(),c.toDouble())
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

    fun geometpdf(a: Number, b: Number): Number {
        val poisson = GeometricDist( a.toDouble())
        return poisson.barF(b.toInt())
    }

    // FIXME not right result
    fun geometcdf(p: Number, n: Number): Number {
        val result = (1- (1-p.toDouble())).pow(n.toDouble())
        return result
    }

    fun SortA(array: Array<Number>): Array<Number>{
        return array.apply { sort()}
    }

    fun SortD(array: Array<Number>): Array<Number>{
        return array.apply { sort()}.reversedArray()
    }



}



