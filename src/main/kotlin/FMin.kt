/**
 *
 *
 * This class was translated by a statistician from the FORTRAN
 * version of fmin.  It is NOT an official translation.  When
 * public domain Java optimization routines become available
 * from professional numerical analysts, then **THE CODE PRODUCED
 * BY THE NUMERICAL ANALYSTS SHOULD BE USED**.
 *
 *
 *
 *
 * Meanwhile, if you have suggestions for improving this
 * code, please contact Steve Verrill at steve@ws13.fpl.fs.fed.us.
 *
 * @author Steve Verrill
 * @version .5 --- March 24, 1998
 */
object FMin  {
    /**
     *
     *
     * This method performs a 1-dimensional minimization.
     * It implements Brent's method which combines a golden-section
     * search and parabolic interpolation.  The introductory comments from
     * the FORTRAN version are provided below.
     *
     *
     * This method is a translation from FORTRAN to Java of the Netlib
     * function fmin.  In the Netlib listing no author is given.
     *
     *
     * Translated by Steve Verrill, March 24, 1998.
     *
     * @param a        Left endpoint of initial interval
     * @param b        Right endpoint of initial interval
     * @param minclass A class that defines a method, f_to_minimize,
     * to minimize.  The class must implement
     * the Fmin_methods interface (see the definition
     * in Fmin_methods.java).  See FminTest.java
     * for an example of such a class.
     * f_to_minimize must have one
     * double valued argument.
     * @param tol      Desired length of the interval in which
     * the minimum will be determined to lie
     * (This should be greater than, roughly, 3.0e-8.)
     */
    fun fMin(a: Double, b: Double, minclass: Fmin_methods, tol: Double): Double {

        /*
        Here is a copy of the Netlib documentation:
        c
        c      An approximation x to the point where f attains a minimum on
        c  the interval (ax,bx) is determined.
        c
        c  input..
        c
        c  ax    left endpoint of initial interval
        c  bx    right endpoint of initial interval
        c  f     function subprogram which evaluates f(x) for any x
        c        in the interval (ax,bx)
        c  tol   desired length of the interval of uncertainty of the final
        c        result (.ge.0.)
        c
        c  output..
        c
        c  fmin  abcissa approximating the point where  f  attains a
        c        minimum
        c
        c     The method used is a combination of golden section search and
        c  successive parabolic interpolation.  Convergence is never much slower
        c  than that for a Fibonacci search.  If f has a continuous second
        c  derivative which is positive at the minimum (which is not at ax or
        c  bx), then convergence is superlinear, and usually of the order of
        c  about 1.324.
        c     The function f is never evaluated at two points closer together
        c  than eps*abs(fmin)+(tol/3), where eps is approximately the square
        c  root of the relative machine precision.  If f is a unimodal
        c  function and the computed values of f are always unimodal when
        c  separated by at least eps*abs(x)+(tol/3), then fmin approximates
        c  the abcissa of the global minimum of f on the interval (ax,bx) with
        c  an error less than 3*eps*abs(fmin)+tol.  If f is not unimodal,
        c  then fmin may approximate a local, but perhaps non-global, minimum to
        c  the same accuracy.
        c      This function subprogram is a slightly modified version of the
        c  Algol 60 procedure localmin given in Richard Brent, Algorithms For
        c  Minimization Without Derivatives, Prentice-Hall, Inc. (1973).
        c
        */
        var a = a
        var b = b
        val c: Double
        var d: Double
        var e: Double
        var eps: Double
        var xm: Double
        var p: Double
        var q: Double
        var r: Double
        var tol1: Double
        var t2: Double
        var u: Double
        var v: Double
        var w: Double
        var fu: Double
        var fv: Double
        var fw: Double
        var fx: Double
        var x: Double
        val tol3: Double
        c = .5 * (3.0 - Math.sqrt(5.0))
        d = 0.0

        // 1.1102e-16 is machine precision
        eps = 1.2e-16
        tol1 = eps + 1.0
        eps = Math.sqrt(eps)
        v = a + c * (b - a)
        w = v
        x = v
        e = 0.0
        fx = minclass.f_to_minimize(x)
        fv = fx
        fw = fx
        tol3 = tol / 3.0
        xm = .5 * (a + b)
        tol1 = eps * Math.abs(x) + tol3
        t2 = 2.0 * tol1

        // main loop
        while (Math.abs(x - xm) > t2 - .5 * (b - a)) {
            r = 0.0
            q = r
            p = q
            if (Math.abs(e) > tol1) {

                // fit the parabola
                r = (x - w) * (fx - fv)
                q = (x - v) * (fx - fw)
                p = (x - v) * q - (x - w) * r
                q = 2.0 * (q - r)
                if (q > 0.0) {
                    p = -p
                } else {
                    q = -q
                }
                r = e
                e = d

                // brace below corresponds to statement 50
            }
            if (Math.abs(p) < Math.abs(.5 * q * r) && p > q * (a - x) && p < q * (b - x)) {

                // a parabolic interpolation step
                d = p / q
                u = x + d

                // f must not be evaluated too close to a or b
                if (u - a < t2 || b - u < t2) {
                    d = tol1
                    if (x >= xm) d = -d
                }

                // brace below corresponds to statement 60
            } else {

                // a golden-section step
                e = if (x < xm) {
                    b - x
                } else {
                    a - x
                }
                d = c * e
            }

            // f must not be evaluated too close to x
            u = if (Math.abs(d) >= tol1) {
                x + d
            } else {
                if (d > 0.0) {
                    x + tol1
                } else {
                    x - tol1
                }
            }
            fu = minclass.f_to_minimize(u)

            // Update a, b, v, w, and x
            if (fx <= fu) {
                if (u < x) {
                    a = u
                } else {
                    b = u
                }

                // brace below corresponds to statement 140
            }
            if (fu <= fx) {
                if (u < x) {
                    b = x
                } else {
                    a = x
                }
                v = w
                fv = fw
                w = x
                fw = fx
                x = u
                fx = fu
                xm = .5 * (a + b)
                tol1 = eps * Math.abs(x) + tol3
                t2 = 2.0 * tol1

                // brace below corresponds to statement 170
            } else {
                if (fu <= fw || w == x) {
                    v = w
                    fv = fw
                    w = u
                    fw = fu
                    xm = .5 * (a + b)
                    tol1 = eps * Math.abs(x) + tol3
                    t2 = 2.0 * tol1
                } else if (fu > fv && v != x && v != w) {
                    xm = .5 * (a + b)
                    tol1 = eps * Math.abs(x) + tol3
                    t2 = 2.0 * tol1
                } else {
                    v = u
                    fv = fu
                    xm = .5 * (a + b)
                    tol1 = eps * Math.abs(x) + tol3
                    t2 = 2.0 * tol1
                }
            }

            // brace below corresponds to statement 190
        }
        return x
    }
}