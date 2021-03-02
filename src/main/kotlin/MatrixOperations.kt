import java.lang.IllegalArgumentException
import java.text.MessageFormat

object MatrixOperations {
    /**
     * Method that multiplies two matrices and returns the result
     *
     * @param x first matrix
     * @param y second matrix
     *
     * @return result after multiplication
     */
    fun multiplyMatrices(x: Array<IntArray>, y: Array<IntArray>): Array<IntArray> {
        val result: Array<IntArray>
        val xRows: Int = x.size
        val xColumns: Int = x[0].size
        val yRows: Int = y.size
        val yColumns: Int = y[0].size
        result = Array(xRows) { IntArray(yColumns) }
        require(xColumns == yRows) { MessageFormat.format("Matrices don't match: {0} != {1}.", xColumns, yRows) }
        for (i in 0 until xRows) {
            for (j in 0 until yColumns) {
                for (k in 0 until xColumns) {
                    result[i][j] += x[i][k] * y[k][j]
                }
            }
        }
        return result
    }

    /**
     * Method that calculates determinant of given matrix
     *
     * @param matrix matrix of which we need to know determinant
     *
     * @return determinant of given matrix
     */
    fun matrixDeterminant(matrix: Array<DoubleArray>): Double {
        var temporary: Array<DoubleArray>
        var result = 0.0
        if (matrix.size == 1) {
            result = matrix[0][0]
            return result
        }
        if (matrix.size == 2) {
            result = matrix[0][0] * matrix[1][1] - matrix[0][1] * matrix[1][0]
            return result
        }
        for (i in 0 until matrix[0].size) {
            temporary = Array(matrix.size - 1) { DoubleArray(matrix[0].size - 1) }
            for (j in 1 until matrix.size) {
                for (k in 0 until matrix[0].size) {
                    if (k < i) {
                        temporary[j - 1][k] = matrix[j][k]
                    } else if (k > i) {
                        temporary[j - 1][k - 1] = matrix[j][k]
                    }
                }
            }
            result += matrix[0][i] * Math.pow(-1.0, i.toDouble()) * matrixDeterminant(temporary)
        }
        return result
    }

    fun invertMatrix(matrix: Array<DoubleArray>): Array<DoubleArray> {
        val auxiliaryMatrix: Array<DoubleArray> = Array(matrix.size) { DoubleArray(matrix.size) }
        val invertedMatrix: Array<DoubleArray> = Array(matrix.size) { DoubleArray(matrix.size) }
        val index: IntArray = IntArray(matrix.size)
        for (i in matrix.indices) {
            auxiliaryMatrix[i][i] = 1.0
        }
        transformToUpperTriangle(matrix, index)
        for (i in 0 until matrix.size - 1) {
            for (j in i + 1 until matrix.size) {
                for (k in matrix.indices) {
                    auxiliaryMatrix[index[j]][k] -= matrix[index[j]][i] * auxiliaryMatrix[index[i]][k]
                }
            }
        }
        for (i in matrix.indices) {
            invertedMatrix[matrix.size - 1][i] = auxiliaryMatrix[index[matrix.size - 1]][i] / matrix[index[matrix.size - 1]][matrix.size - 1]
            for (j in matrix.size - 2 downTo 0) {
                invertedMatrix[j][i] = auxiliaryMatrix[index[j]][i]
                for (k in j + 1 until matrix.size) {
                    invertedMatrix[j][i] -= matrix[index[j]][k] * invertedMatrix[k][i]
                }
                invertedMatrix[j][i] /= matrix[index[j]][j]
            }
        }
        return invertedMatrix
    }

    fun transformToUpperTriangle(matrix: Array<DoubleArray>, index: IntArray) {
        var c0: Double
        var c1: Double
        var pi0: Double
        var pi1: Double
        var pj: Double
        var itmp: Int
        var k: Int
        val c: DoubleArray = DoubleArray(matrix.size)
        for (i in matrix.indices) {
            index[i] = i
        }
        for (i in matrix.indices) {
            c1 = 0.0
            for (j in matrix.indices) {
                c0 = Math.abs(matrix[i][j])
                if (c0 > c1) {
                    c1 = c0
                }
            }
            c[i] = c1
        }
        k = 0
        for (j in 0 until matrix.size - 1) {
            pi1 = 0.0
            for (i in j until matrix.size) {
                pi0 = Math.abs(matrix[index[i]][j])
                pi0 /= c[index[i]]
                if (pi0 > pi1) {
                    pi1 = pi0
                    k = i
                }
            }
            itmp = index[j]
            index[j] = index[k]
            index[k] = itmp
            for (i in j + 1 until matrix.size) {
                pj = matrix[index[i]][j] / matrix[index[j]][j]
                matrix[index[i]][j] = pj
                for (l in j + 1 until matrix.size) {
                    matrix[index[i]][l] -= pj * matrix[index[j]][l]
                }
            }
        }
    }

    /**
     * Method that prints matrix
     *
     * @param matrix matrix to print
     * @param id     what does the matrix contain?
     */
    fun printMatrix(matrix: Array<IntArray>, id: Int) {
        val doubleMatrix = Array(matrix.size) { DoubleArray(matrix[0].size) }
        for (i in matrix.indices) {
            for (j in 0 until matrix[i].size) {
                doubleMatrix[i][j] = matrix[i][j].toDouble()
            }
        }
        printMatrix(doubleMatrix, id)
    }

    /**
     * Method that prints matrix
     *
     * @param matrix matrix to print
     * @param id     what does the matrix contain?
     */
    fun printMatrix(matrix: Array<DoubleArray>, id: Int) {
        val rows: Int = matrix.size
        val cols: Int = matrix[0].size
        when (id) {
            1 -> print(MessageFormat.format("First matrix[{0}][{1}]:", rows, cols))
            2 -> print(MessageFormat.format("Second matrix[{0}][{1}]:", rows, cols))
            3 -> print(MessageFormat.format("Result[{0}][{1}]:", rows, cols))
            4 -> print(MessageFormat.format("Inverted matrix[{0}][{1}]:", rows, cols))
            else -> print(MessageFormat.format("Matrix[{0}][{1}]:", rows, cols))
        }
        println()
        for (i in matrix.indices) {
            print("[")
            for (j in matrix[i].indices) {
                print(matrix[i][j])
                if (j + 1 != matrix[i].size) {
                    print(", ")
                }
            }
            if (i + 1 != matrix.size) {
                println("]")
            } else {
                println("].")
            }
        }
        println()
    }

    fun transposeMatrix(m: Array<DoubleArray>): Array<DoubleArray> {
        val temp = Array(m[0].size) { DoubleArray(m.size) }
        for (i in m.indices) for (j in 0 until m[0].size) temp[j][i] = m[i][j]
        return temp
    }

    fun rref(matrix: Array<DoubleArray>): Array<DoubleArray> {
        var matrix = matrix
        var lead = 0
        var i: Int

        // number of rows and columns in matrix
        val numRows = matrix.size
        val numColumns: Int = matrix[0].size
        for (k in 0 until numRows) {
            if (numColumns <= lead) {
                break
            }
            i = k
            while (matrix[i][lead] == 0.0) {
                i++
                if (numRows == i) {
                    i = k
                    lead++
                    if (numColumns == lead) {
                        break
                    }
                }
            }
            matrix = rowSwap(matrix, i, k)
            if (matrix[k][lead] != 0.0) {
                matrix = rowScale(matrix, k, 1 / matrix[k][lead])
            }
            i = 0
            while (i < numRows) {
                if (i != k) {
                    matrix = rowAddScale(matrix, k, i, -1 * matrix[i][lead])
                }
                i++
            }
            lead++
        }
        return matrix
    }


    fun ref(matrix: Array<DoubleArray>): Array<DoubleArray> {
        var matrix = matrix
        var lead = 0
        var i: Int

        val list = mutableListOf<DoubleArray>()

        // number of rows and columns in matrix
        val numRows = matrix.size
        val numColumns: Int = matrix[0].size
        for (k in 0 until numRows) {
            if (numColumns <= lead) {
                break
            }
            i = k
            while (matrix[i][lead] == 0.0) {
                i++
                if (numRows == i) {
                    i = k
                    lead++
                    if (numColumns == lead) {
                        break
                    }
                }
            }

            matrix = rowSwap(matrix, i, k)

            if (matrix[k][lead] != 0.0) {
                matrix = rowScale(matrix, k, 1 / matrix[k][lead])
            }
            i = 0
            while (i < numRows) {
                if (i != k) {
                    matrix = rowAddScale(matrix, k, i, -1 * matrix[i][lead])
                }
                i++
            }
            printMatrix(matrix, 1)
            val listOfNumber = mutableListOf<Double>()
            matrix[k].forEach {
                listOfNumber.add(it)

            }
            list.add(listOfNumber.toDoubleArray())
            lead++
        }
        return list.toTypedArray()
    }

    /**
     * Swap positions of 2 rows
     *
     * @param matrix matrix before row additon
     * @param rowIndex1 int index of row to swap
     * @param rowIndex2 int index of row to swap
     *
     * @return matrix after row swap
     */
    private fun rowSwap(matrix: Array<DoubleArray>, rowIndex1: Int,
                        rowIndex2: Int): Array<DoubleArray> {
        // number of columns in matrix
        val numColumns: Int = matrix[0].size

        // holds number to be swapped
        var hold: Double
        for (k in 0 until numColumns) {
            hold = matrix[rowIndex2][k]
            matrix[rowIndex2][k] = matrix[rowIndex1][k]
            matrix[rowIndex1][k] = hold
        }
        return matrix
    }

    /**
     * Adds 2 rows together row2 = row2 + row1
     *
     * @param matrix matrix before row additon
     * @param rowIndex1 int index of row to be added
     * @param rowIndex2 int index or row that row1 is added to
     *
     * @return matrix after row addition
     */
    private fun rowAdd(matrix: Array<DoubleArray>, rowIndex1: Int,
                       rowIndex2: Int): Array<DoubleArray> {
        // number of columns in matrix
        val numColumns: Int = matrix[0].size
        for (k in 0 until numColumns) {
            matrix[rowIndex2][k] += matrix[rowIndex1][k]
        }
        return matrix
    }

    /**
     * Multiplies a row by a scalar
     *
     * @param matrix matrix before row additon
     * @param rowIndex int index of row to be scaled
     * @param scalar double to scale row by
     *
     * @return matrix after row scaling
     */
    private fun rowScale(matrix: Array<DoubleArray>, rowIndex: Int,
                         scalar: Double): Array<DoubleArray> {
        // number of columns in matrix
        val numColumns: Int = matrix[0].size
        for (k in 0 until numColumns) {
            matrix[rowIndex][k] *= scalar
        }
        return matrix
    }

    /**
     * Adds a row by the scalar of another row
     * row2 = row2 + (row1 * scalar)
     * @param matrix matrix before row additon
     * @param rowIndex1 int index of row to be added
     * @param rowIndex2 int index or row that row1 is added to
     * @param scalar double to scale row by
     *
     * @return matrix after row addition
     */
    private fun rowAddScale(matrix: Array<DoubleArray>, rowIndex1: Int,
                            rowIndex2: Int, scalar: Double): Array<DoubleArray> {
        // number of columns in matrix
        val numColumns: Int = matrix[0].size
        for (k in 0 until numColumns) {
            matrix[rowIndex2][k] += matrix[rowIndex1][k] * scalar
        }
        return matrix
    }
}