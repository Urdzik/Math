import cern.jet.math.Functions
import org.mariuszgromada.math.mxparser.Expression
import org.mariuszgromada.math.mxparser.mXparser
import java.util.function.DoubleFunction

//fun main(args: Array<String>) {
////    val matrix = arrayOf(doubleArrayOf(-2.0, -4.0, 0.0, -6.0), doubleArrayOf(1.0, 2.0, -6.0, 0.0), doubleArrayOf(-2.0, 4.0, -6.0, -4.0))
//
////    val result = Taculator.nDeriv(
//////        arrayOf(1,3,4,8),
//////        arrayOf(1,2,2,5)
////    )
////    println(result)
//}


    // approximate the limit
    private const val DX = 0.0001

    /**

     */
    private fun derive(f: DoubleFunction<Double>): DoubleFunction<Double> {
        return DoubleFunction { x: Double ->
            (f.apply(x + DX) - f.apply(
                x
            )) / DX
        }
    }

    fun main(args: Array<String>) {
      val result = Taculator.fMin("x^3-2*x-2", -5, 5)
        println(result)


//        val x = DerivativeStructure(1, 3, 0, 1.0)
//
//        println(x.value)
//        // Basically, x --> x^2.
//        // Basically, x --> x^2.
//        val x2: DerivativeStructure = x.pow(2)
//        //Linear combination: y = 4x^2 +
//
//        /*Scanner scan = new Scanner(System.in);
//    System.out.println("Input function:");
//    String function=scan.nextLine();*/
//
//        //Linear combination: y = 4x^2 + 2x
//
//        /*Scanner scan = new Scanner(System.in);
//    System.out.println("Input function:");
//    String function=scan.nextLine();*/
//        val y = DerivativeStructure(4.0, x2, 2.0, x)
//        println("y    = " + y.value)
//        println("y'   = " + y.getPartialDerivative(1))
//        println("y''  = " + y.getPartialDerivative(2))
//        println("y''' = " + y.getPartialDerivative(3))

    }
