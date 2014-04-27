package s99

object Logic1 {

  // TODO P46 (**) Truth tables for logical expressions.
  def and(a: Boolean, b: Boolean) = (a, b) match {
    case (true, true) => true
    case (_, _) => false
  }

  def not(a: Boolean) = a match {
    case true => false
    case false => true
  }

  def or(a: Boolean, b: Boolean) = (a, b) match {
    case (true, _) => true
    case (_, true) => true
    case (_, _) => false
  }

  def equ(a: Boolean, b: Boolean) = or(and(a, b), and(not(a), not(b)))

  def xor(a: Boolean, b: Boolean) = (a, b) match {
    case (true, false) => true
    case (false, true) => true
    case (_, _) => false
  }

  def table2(f: (Boolean, Boolean) => Boolean) = {
    for (a <- List(true, false); b <- List(true, false)) {
      val v = f(a, b)
      println(s"$a $b $v")
    }
  }

  // TODO P47 (*) Truth tables for logical expressions (2).

  // TODO P49 (**) Gray code.

  // TODO P50 (***) Huffman code.
}