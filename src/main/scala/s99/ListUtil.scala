package s99

import scala.collection.immutable.List
import scala.collection.immutable.Nil
import java.util.NoSuchElementException

object ListUtil {

  /**
   * Return the last item of the given list.
   */
  def last[A](l: List[A]) = l match {
    case Nil => throw new NoSuchElementException
    case _ => l.last
  }

  /**
   * Return the last but one element of a list.
   */
  def penultimate[A](l: List[A]) = l match {
    case Nil => throw new NoSuchElementException
    case head :: Nil => throw new NoSuchElementException
    case _ => l.takeRight(2).head
  }

  /**
   * Return the last but one element of a list using a recursive approach.
   */
  def penultimateRecursive[A](l: List[A]): A = l match {
    case h :: x :: Nil => h
    case x :: tail => penultimateRecursive(tail)
    case Nil => throw new NoSuchElementException
  }

  /**
   * Return the n element in the list.
   */
  def nth[A](i: Int, l: List[A]): A = l match {
    case x :: tial => i match {
      case 0 => l.head
      case _ => nth(i - 1, tial)
    }
    case Nil => throw new NoSuchElementException
  }

  /**
   * Return the number of element in the list using a recursive approach.
   * instead of using list.length
   */
  def lengthRecoursive[A](l: List[A]): Int = l match {
    case x :: tail => 1 + lengthRecoursive(tail)
    case _ => 0
  }

  def lengthNative[A](l: List[A]) = {
    l.length
  }

  /**
   * Return the reversed list using a recursive approach
   * instead of using list.reverse
   */
  def reverseList[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case x :: tail => reverseList(tail) ::: List(x)
  }

  def reverseListNative[A](l: List[A]) {
    l.reverse
  }

  /**
   * Detect whether a list if a palindrome
   */
  def palindrome[A](l: List[A]): Boolean = {
    val halfIndex = l.length / 2
    val listHead = l.take(halfIndex)
    val listTail = l.takeRight(halfIndex).reverse
    listHead == listTail
  }

  /**
   * Return a flatten list
   */
  def flattenList[A](l: List[List[A]]): List[A] = {
    l.flatten
  }

  /**
   * Return a flatten list of a list of list.
   */
  def flattenAny(l: List[Any]): List[Any] = l flatMap {
    case x: List[Any] => flattenAny(x)
    case item => List(item)
  }

  /**
   * Remove duplicate elements in the list.
   * If a list contains repeated elements they should be replaced with a single copy of the element. The order of the elements should not be changed.
   * Ex: List(1,1,2,2,3) -> List(1,2,3)
   */
  def removeDuplicate[A](l: List[A]): List[A] = l match {
    case x :: y :: tail =>
      if (x == y) removeDuplicate(x :: tail) else x :: removeDuplicate(y :: tail)
    case _ => l
  }

  /**
   * Pack consecutive duplicates of list elements into sublists.
   * The method packDuplicate is a better implementation.
   *
   * Example:
   *  For: List(1, 2, 2, 2, 2, 2, 2, 2, 2, 3, 3, 2, 4, 4)
   *  Res: List(1, List(2, 2, 2, 2, 2, 2, 2, 2), List(3, 3), 2, List(4, 4)
   *
   */
  def packDuplicateList(l: List[Any]): List[Any] = l match {
    case x :: y :: tail =>
      if (x == y)
        packDuplicateList(List(x, y) :: tail)
      else
        x match {
          case list: List[Any] => {
            if (list.contains(y)) {
              packDuplicateList((list :+ y) :: tail)
            } else {
              x :: packDuplicateList(y :: tail)
            }
          }
          case _ => x :: packDuplicateList(y :: tail)

        }
    case _ => l
  }

  /**
   * Pack consecutive duplicates of list elements into sublists.
   * Example:
   *  For: List(1, 2, 2, 2, 2, 2, 2, 2, 2, 3, 3, 2, 4, 4)
   *  Res: List(1, List(2, 2, 2, 2, 2, 2, 2, 2), List(3, 3), 2, List(4, 4)
   */
  def packDuplicate(l: List[Any]): List[Any] = {
    if (l.isEmpty) Nil
    else {
      val (duplicated, tail) = l.span(_ == l.head)
      if (duplicated.size == 1) duplicated.head :: packDuplicate(tail)
      else duplicated :: packDuplicate(tail)
    }
  }

  /**
   * P10 (*) Run-length encoding of a list.
   * Use the result of problem P09 to implement the so-called run-length encoding data compression method.
   *  Consecutive duplicates of elements are encoded as tuples (N, E) where N is the number of duplicates of the element E.
   */
  def encode[A](l: List[List[A]]) = {
    l.map(l => (l.size, l.head))
  }

  /**
   * P11 (*) Modified run-length encoding.
   * Modify the result of problem P10 in such a way that if an element has no duplicates it is simply copied into the result list.
   * Only elements with duplicates are transferred as (N, E) terms.
   * Example:
   * scala> encodeModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
   * res0: List[Any] = List((4,'a), 'b, (2,'c), (2,'a), 'd, (4,'e))
   */
  def encodeModified[A](list: List[A]) = {
    val packed = packDuplicate(list)

    packed map (l => l match {
      case list: List[Any] => (list.size, list.head)
      case _ => l

    })

  }

  /**
   *  P12 (**) Decode a run-length encoded list.
   *  Given a run-length code list generated as specified in problem P10,
   *   construct its uncompressed version.
   *  Example:
   *   scala> decode(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e)))
   *   res0: List[Symbol] = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
   */
  def decode(encoded: List[(Int, Any)]) = {
    val expanded = encoded.map(x => List.fill(x._1)(x._2))
    expanded.flatten
  }

  /**
   * Another implementation for P12 (**) Decode a run-length encoded list.
   * Example:
   *   scala> decode(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e)))
   *   res0: List[Symbol] = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
   */
  def decodeFlat(encoded: List[(Int, Any)]) = {
    encoded.flatMap(x => List.fill(x._1)(x._2))
  }

  /**
   * P13 (**) Run-length encoding of a list (direct solution).
   * Implement the so-called run-length encoding data compression method directly.
   *  I.e. don't use other methods you've written (like P09's pack); do all the work directly.
   * Example:
   * scala> encodeDirect(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
   * res0: List[(Int, Symbol)] = List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))
   */
  def encodeDirect[A](l: List[A]): List[(Int, A)] = {
    if (l.isEmpty) Nil
    else {
      val (duplicated, tail) = l.span(_ == l.head)
      (duplicated.length, duplicated.head) :: encodeDirect(tail)
    }
  }

  /**
   * P14 (*) Duplicate the elements of a list.
   * Example:
   * scala> duplicate(List('a, 'b, 'c, 'c, 'd))
   * res0: List[Symbol] = List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd)
   */
  def duplicate[A](l: List[A]) = {
    duplicateN(2, l)
  }

  /**
   * P15 (**) Duplicate the elements of a list a given number of times.
   * Example:
   * scala> duplicateN(2,List('a, 'b, 'c, 'c, 'd))
   * res0: List[Symbol] = List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd)
   */
  def duplicateN[A](times: Int, l: List[A]) = {
    l.flatMap { List.fill(times)(_) }
  }

  /**
   * P16 (**) Drop every Nth element from a list.
   * Example:
   * scala> drop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
   * res0: List[Symbol] = List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k)
   */
  def drop[A](i: Int, l: List[A]) = {
    l.zipWithIndex.filterNot(_._2 % i == 2).unzip._1
  }

  /**
   * P17 (*) Split a list into two parts.
   * The length of the first part is given. Use a Tuple for your result.
   * Example:
   * scala> split(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
   * res0: (List[Symbol], List[Symbol]) = (List('a, 'b, 'c),List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
   */
  def split[A](i: Int, l: List[A]) = {
    l.splitAt(i)
  }

}