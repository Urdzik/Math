class ComplexNumber(val real: Double, val imaginary: Double) {
//    override fun equals(`object`: Any?): Boolean {
//        if (`object` !is ComplexNumber) return false
//        val a = `object`
//        return real == a.real && imaginary == a.imaginary
//    }

    val amount: Double
        get() = Math.sqrt(real * real + imaginary * imaginary)
    val angle: Double
        get() = Math.atan2(imaginary, real)

    fun add(b: ComplexNumber): ComplexNumber {
        return add(this, b)
    }

    fun sub(b: ComplexNumber): ComplexNumber {
        return sub(this, b)
    }

    operator fun div(b: ComplexNumber): ComplexNumber {
        return div(this, b)
    }

    fun mul(b: ComplexNumber): ComplexNumber {
        return mul(this, b)
    }

    fun conjugation(): ComplexNumber {
        return conjugation(this)
    }

    companion object {
        fun createPolar(amount: Double, angel: Double): ComplexNumber {
            return ComplexNumber(amount * Math.cos(angel), amount * Math.sin(angel))
        }

        /**
         * Addition:
         * @param a
         * @param b
         * @return
         */
        private fun add(a: ComplexNumber, b: ComplexNumber): ComplexNumber {
            return ComplexNumber(a.real + b.real, a.imaginary + b.imaginary)
        }

        /**
         * Subtraktion:
         * @param a
         * @param b
         * @return
         */
        private fun sub(a: ComplexNumber, b: ComplexNumber): ComplexNumber {
            return ComplexNumber(a.real - b.real, a.imaginary - b.imaginary)
        }

        /**
         * Multiplikation:
         * @param a
         * @param b
         * @return
         */
        private fun mul(a: ComplexNumber, b: ComplexNumber): ComplexNumber {
            return ComplexNumber(
                a.real * b.real - a.imaginary * b.imaginary,
                a.imaginary * b.real + a.real * b.imaginary
            )
        }

        /**
         * Division:
         * @param a
         * @param b
         * @return
         */
        private fun div(a: ComplexNumber, b: ComplexNumber): ComplexNumber {
            val d = b.real * b.real + b.imaginary * b.imaginary
            return if (d == 0.0) ComplexNumber(Double.NaN, Double.NaN) else ComplexNumber(
                (a.real * b.real + a.imaginary * b.imaginary) / d,
                (a.imaginary * b.real - a.real * b.imaginary) / d
            )
        }

        /**
         * Konjugation:
         * @param a
         * @return
         */
        private fun conjugation(a: ComplexNumber): ComplexNumber {
            return ComplexNumber(a.real, -a.imaginary)
        }
    }
}