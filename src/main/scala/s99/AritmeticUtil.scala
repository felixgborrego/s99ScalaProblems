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

    /**
     *  P35 (**) Determine the prime factors of a given positive integer.
     * Construct a flat list containing the prime factors in ascending order.
     */
    def primeFactors: List[Int] = {
      val factors = primes.filter(number % _ == 0)
      if (factors.isEmpty) {
        Nil
      } else {
        val prime = factors.head
        prime :: (number / prime).primeFactors
      }

    }

    def primes = (2 to number).toStream.filter(_.isPrime)

    /**
     * P36 (**) Determine the prime factors of a given positive integer (2).
     * Construct a list containing the prime factors and their multiplicity.
     * scala> 315.primeFactorMultiplicity
     * res0: List[(Int, Int)] = List((3,2), (5,1), (7,1))
     */
    def primeFactorMultiplicity = {
      primeFactors.groupBy(x => x).toList.map(t => (t._1, t._2.length)).sortBy(t => t._1)
    }
    def primeFactorMultiplicityMap = {
      primeFactors.groupBy(x => x).map(t => (t._1, t._2.length))
    }

  }

  /**
   * P32 (**) Determine the greatest common divisor of two positive integer numbers.
   */
  def gcd(n1: Int, n2: Int): Int = {
    if (n2 == 0) n1 else gcd(n2, n1 % n2)
  }

  /**
   * P39 (*) A list of prime numbers.
   * Given a range of integers by its lower and upper limit, construct a list of all prime numbers in that range.
   * scala> listPrimesinRange(7 to 31)
   * res0: List[Int] = List(7, 11, 13, 17, 19, 23, 29, 31)
   */
  def listPrimesinRange(r: Range) = r.toStream.filter(_.isPrime).toList
}

