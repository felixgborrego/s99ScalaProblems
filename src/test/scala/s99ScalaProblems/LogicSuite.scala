package s99ScalaProblems

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.scalatest._
import s99.Logic1._

/**
 *
 */
class LogicSpec extends FlatSpec with Matchers {

  info("P46 (**) Truth tables for logical expressions.")
  "table2" should "print a truth table for the given expresion " in {
    table2(and)
  }

}