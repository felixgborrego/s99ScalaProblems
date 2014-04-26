package s99ScalaProblems

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.scalatest._
import s99.AritmeticUtils._

/**
 *
 */
class AritmeticUtilSpec extends FlatSpec with Matchers {

  info("P31 (**) Determine whether a given integer number is prime.")
  "1.isPrime" should "return true" in {
    assert(1.isPrime == true)
  }
  "2.isPrime" should "return true" in {
    assert(2.isPrime == true)
  }
  "3.isPrime" should "return true" in {
    assert(3.isPrime == true)
  }
  "6.isPrime" should "return false" in {
    assert(6.isPrime == false)
  }

  info("P32 (**) Determine the greatest common divisor of two positive integer numbers.")
  "gcd(2322,654)" should "return 6" in {
    assert(gcd(2322, 654) == 6)
  }

  info("P33 (*) Determine whether two positive integer numbers are coprime. Two numbers are coprime if their greatest common divisor equals 1.")
  "35.isCoprimeTo(64)" should "return true" in {
    assert(35.isCoprimeTo(64))
  }
  
  info("P34 (**) Calculate Euler's totient function phi(m).")
   "10.totient" should "return 4" in {
     assert( 10.totient == 4)
  }   

}