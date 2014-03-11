package s99ScalaProblems

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.scalatest._
import s99.ListUtil._

/**
 *
 */
class ListUtilSpec extends FlatSpec with Matchers {

  info("P01 / Find the last element of a list.")
  "last" should "throw NoSuchElementException if an empty list is popped" in {
    a[NoSuchElementException] should be thrownBy {
      last(List[String]());
    }
  }

  "last" should "return the last element if a non empty list is popped" in {
    assert(last(List(1)) == 1)
    assert(last(List("hi", "test")) == "test")
  }

  info("P02 / Find the last but one element of a list.")
  "penultimate" should "throw NoSuchElementException if an empty list is popped" in {
    a[NoSuchElementException] should be thrownBy {
      penultimate(List[String]());
      penultimateRecursive(List[String]())
    }
  }

  "penultimate" should "throw NoSuchElementException if a list with only one element is popped" in {
    a[NoSuchElementException] should be thrownBy {
      penultimate(List(1));

      penultimateRecursive(List(1));
    }
  }

  "penultimate" should "return the last but one element in the list" in {
    assert(penultimate(List(1, 2)) == 1);
    assert(penultimate(List(1, 2, 3, 4)) == 3);

    assert(penultimateRecursive(List(1, 2)) == 1);
    assert(penultimateRecursive(List(1, 2, 3, 4)) == 3);
  }

  info("P03 / Find the Kth element of a list.")
  "kth method" should "throw NoSuchElementException if the list is empty" in {
    a[NoSuchElementException] should be thrownBy {
      nth(0, List[String]())
    }
  }
  "kth method" should "throw NoSuchElementException if the size of the list is smaller or eaual than k" in {
    a[NoSuchElementException] should be thrownBy {
      nth(1, List("test"))
    }
    a[NoSuchElementException] should be thrownBy {
      nth(3, List("test"))
    }
  }

  "kth method" should "return the k element in the list" in {
    assert(nth(1, List("test", "test2")) == "test2")
    assert(nth(0, List("test0", "test2")) == "test0")
  }

  info("P04 (*) Find the number of elements of a list.")
  "length" should "return the number of element of the list" in {
    assert(lengthRecoursive(Nil) == 0)
    assert(lengthRecoursive(1 :: 1 :: 1 :: 1 :: 1 :: Nil) == 5)
    assert(lengthNative(1 :: 1 :: 1 :: 1 :: 1 :: Nil) == 5)
  }

  info("P05 (*) Reverse a list.")
  "reverse" should "return a new list with a reverse order" in {
    assert(reverseList(1 :: 2 :: Nil) == 2 :: 1 :: Nil)
    assert(reverseList(Nil) == Nil)
  }

  info("P06 (*) Find out whether a list is a palindrome.")
  "palindrome" should "find out whether a list is a palindrome and return true or false" in {
    assert(palindrome(List(1, 2)) == false)
    assert(palindrome(List(1, 2, 3, 2, 1)) == true)
    assert(palindrome(List(3)) == true)
  }

  info("P07 (**) Flatten a nested list structure.")
  "flattenList" should "flatten a list of list into a list" in {
    val listOfList = List(List(1, 2), List(3, 4))
    val flatList = List(1, 2, 3, 4)
    assert(flattenList(listOfList) == flatList)
  }

  "flattenAny" should "flatten a list of any into a list" in {
    val listOfList = List(List(1, 2), List(3, List(4, 5)))
    val flatList = List(1, 2, 3, 4, 5)
    assert(flattenAny(listOfList) == flatList)
  }

  info("P08 (**) Eliminate consecutive duplicates of list elements.")
  "removeDuplicate" should "eliminate consecutive duplicates of a list of elements" in {
    val list = List(1, 1, 1, 2, 2, 2, 3, 3)
    val listClean = List(1, 2, 3)
    assert(removeDuplicate(list) == listClean)
  }

  info("P09 (**) Pack consecutive duplicates of list elements into sublists.")
  "packDuplicateList" should "pack in a list consecutive duplicates of a list of elements" in {
    var list = List(1, 1, 1, 2, 2, 2, 3, 3, 4, 1)
    var packed = List(List(1, 1, 1), List(2, 2, 2), List(3, 3), 4, 1)
    assert(packDuplicateList(list) == packed)
    assert(packDuplicate(list) == packed)

    list = List(1, 2, 2, 2, 2, 2, 2, 2, 2, 3, 3, 2, 4, 4)
    packed = List(1, List(2, 2, 2, 2, 2, 2, 2, 2), List(3, 3), 2, List(4, 4))
    assert(packDuplicateList(list) == packed)
    assert(packDuplicate(list) == packed)
  }

  info("P10 (*) Run-length encoding of a list.")
  "encode" should "implement a so-called run-length encoding data compression method" in {
    var packed = List(List(1, 1, 1), List(2, 2, 2), List(3, 3))
    var encoded = List((3, 1), (3, 2), (2, 3))
    assert(encode(packed) == encoded)
  }

  info("P11 (*) Modified run-length encoding.")
  "encodeModified" should "implement a so-called run-length encoding data compression method, but if an element has no duplicates it is simply copied into the result list" in {
    val encoded = encodeModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
    assert(encoded == List((4, 'a), 'b, (2, 'c), (2, 'a), 'd, (4, 'e)))
  }

  info("P12 (**) Decode a run-length encoded list.")
  "decode" should "decode the encoded list" in {
    val encoded = List((3, 'a'), (3, 'b'), (2, 'c'))
    assert(decode(encoded) == List('a', 'a', 'a', 'b', 'b', 'b', 'c', 'c'))
    assert(decodeFlat(encoded) == List('a', 'a', 'a', 'b', 'b', 'b', 'c', 'c'))
  }

  info("P13 (**) Run-length encoding of a list (direct solution)..")
  "encodeDirect" should "perform a Run-length encoding of a list" in {
    val encoded = encodeDirect(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
    assert(encoded == List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e)))
  }

  info("P14 (*) Duplicate the elements of a list.")
  info("P15 (**) Duplicate the elements of a list a given number of times.")
  "duplicate" should "perform a Run-length encoding of a list" in {
    val duplicated = duplicate(List('a, 'b, 'c, 'c, 'd))
    assert(duplicated == List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd))
    val duplicatedN = duplicateN(3, List('a, 'b, 'c, 'c, 'd))
    assert(duplicatedN == List('a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c, 'c, 'c, 'c, 'd, 'd, 'd))
  }

  info("P16 (**) Drop every Nth element from a list.")
  "drop" should "drop every Nth element from a list." in {
    val droped = drop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
    assert(droped == List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k))
  }

  info("P17 (*) Split a list into two parts.")
  "split" should "split a list in two parts." in {
    val (splited1, splited2) = split(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
    assert(splited1 == List('a, 'b, 'c))
    assert(splited2 == List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
  }

  info("18 (**) Extract a slice from a list.")
  "slice" should "extrat a slice from a list" in {
    val sliced = slice(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
    assert(sliced == List('d, 'e, 'f, 'g))
  }

  info("P19 (**) Rotate a list N places to the left.")
  "rotate" should "rotate a list N paces to the left" in {
    val rotated1 = rotate(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
    assert(rotated1 == List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'a, 'b, 'c))

    val rotated2 = rotate(-2, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
    assert(rotated2 == List('j, 'k, 'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i))

  }

  info("P20 (*) Remove the Kth element from a list.")
  "removeAt" should "remove the kth element from a list" in {
    val removed = removeAt(1, List('a, 'b, 'c, 'd))
    assert(removed == (List('a, 'c, 'd), 'b))
  }

  info("P21 (*) Insert an element at a given position into a list.")
  "insertAt" should "remove the kth element from a list" in {
    val inserted = insertAt('new, 1, List('a, 'b, 'c, 'd))
    assert(inserted == List('a, 'new, 'b, 'c, 'd))
  }

  info("P22 (*) Create a list containing all integers within a given range.")
  "range" should "create a list containing all integers with a given range" in {
    val ranged = rangeBuildIn(4, 9)
    assert(ranged == List(4, 5, 6, 7, 8, 9))
  }

  info("P23 (**) Extract a given number of randomly selected elements from a list.")
  "randomSelect" should "extrat a given number of randomly elements" in {
    val selected = randomSelect(3, List('a, 'b, 'c, 'd, 'f, 'g, 'h))
    assert(selected.length == 3)
  }

  info("P24 (*) Lotto: Draw N different random numbers from the set 1..M.")
  "lotto" should "return N different random number from the list set 1..M" in {
    val list = lotto(6, 49)
    assert(list.length == 6)
  }

  info("P25 (*) Generate a random permutation of the elements of a list.")
  "randomPermute" should "generate a random permutation of the elements of a list" in {
    val list = randomPermute(List('a, 'b, 'c, 'd, 'e, 'f))
    assert(list.length == 6)
  }

}