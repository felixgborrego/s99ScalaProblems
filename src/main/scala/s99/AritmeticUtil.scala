package s99
import scala.util.control.Breaks._
import scala.collection.immutable.List._
import scala.collection.immutable.Range._
import Stream._

object AritmeticUtils {

  implicit class AritmeticImplicits(number: Int) {

    /**
     * P31 (**) Determine whether a given integer number is prime.
     */
    def isPrime = {
      var r = (2 to (number - 1))
      (2 to (number - 1)).forall(number % _ != 0)
    }

    /**
     * P33 (*) Determine whether two positive integer numbers are coprime.
     * Two numbers are coprime if their greatest common divisor equals 1.
     */
    def isCoprimeTo(n: Int) = gcd(number, n) == 1

    /**
     * P34 (**) Calculate Euler's totient function phi(m).
     * Euler's so-called totient function phi(m) is defined as the number of positive integers r (1 <= r <= m) that are coprime to m.
     *
     */
    def totient = (1 to number).toStream.filter(_.isCoprimeTo(number)).size

  }

  /**
   * P32 (**) Determine the greatest common divisor of two positive integer numbers.
   */
  def gcd(n1: Int, n2: Int): Int = {
    if (n2 == 0) n1 else gcd(n2, n1 % n2)
  }

}

