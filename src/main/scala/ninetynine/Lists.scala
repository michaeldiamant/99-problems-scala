package ninetynine

import scala.annotation.tailrec

object Lists {

  // P01 (*) Find the last element of a list.
  //     Example:
  //     scala> last(List(1, 1, 2, 3, 5, 8))
  //     res0: Int = 8
  object P01 {

    def last[E](xs: List[E]): E = {
      @tailrec
      def findLast(xss: List[E]): E =
        xss match {
          case Nil => sys.error("cannot find last of empty list")
          case h :: Nil => h
          case _ :: tail => findLast(tail)
        }

      findLast(xs)
    }
  }

  // P02 (*) Find the last but one element of a list.
  //     Example:
  //     scala> penultimate(List(1, 1, 2, 3, 5, 8))
  //     res0: Int = 5
  object P02 {

    def penultimate[E](xs: List[E]): E = {
      @tailrec
      def findPenultimate(xss: List[E]): E =
        xss match {
          case Nil => sys.error("cannot find penultimate of empty list")
          case h :: _ :: Nil => h
          case _ :: tail => findPenultimate(tail)
        }

      findPenultimate(xs)
    }

    def penultimateBuiltIn[E](xs: List[E]): E =
      xs.reverse.tail.head
  }

  // P03 (*) Find the Kth element of a list.
  //     By convention, the first element in the list is element 0.
  //
  //     Example:
  //     scala> nth(2, List(1, 1, 2, 3, 5, 8))
  //     res0: Int = 2

  object P03 {

    def nth[E](n: Int, xs: List[E]): E = {
      @tailrec
      def findNth(xss: List[E], remaining: Int): E =
        remaining match {
          case 0 => xss.head
          case _ => findNth(xss.tail, remaining - 1)
        }
      findNth(xs, n)
    }

    def nthBuiltIn[E](n: Int, xs: List[E]): E =
      xs.drop(n).head
  }

  // P04 (*) Find the number of elements of a list.
  //     Example:
  //     scala> length(List(1, 1, 2, 3, 5, 8))
  //     res0: Int = 6
  object P04 {

    def length[E](xs: List[E]): Int = {
      @tailrec
      def findLength(xss: List[E], size: Int): Int =
        xss match {
          case Nil => size
          case _ :: tail => findLength(tail, size + 1)
        }

      findLength(xs, 0)
    }

    def lengthBuiltIn[E](xs: List[E]): Int =
      xs.foldLeft(0)((acc, _) => acc + 1)
  }

  // P05 (*) Reverse a list.
  //     Example:
  //     scala> reverse(List(1, 1, 2, 3, 5, 8))
  //     res0: List[Int] = List(8, 5, 3, 2, 1, 1)
  object P05 {

    def reverse[E](xs: List[E]): List[E] = {
      @tailrec
      def doReverse(originals: List[E], reverses: List[E]): List[E] =
        originals match {
          case Nil => reverses
          case h :: t => doReverse(t, h :: reverses)
        }

      doReverse(xs, Nil)
    }

    def reverseBuiltIn[E](xs: List[E]): List[E] =
      xs.foldLeft(List.empty[E])((acc, e) => e :: acc)
  }

  // P06 (*) Find out whether a list is a palindrome.
  //     Example:
  //     scala> isPalindrome(List(1, 2, 3, 2, 1))
  //     res0: Boolean = true
  object P06 {

    def isPalindrome[E](xs: List[E]): Boolean =
      xs == xs.reverse

    def isPalindromeFaster[E](xs: List[E]): Boolean = {
      // Ignore middle element when size is odd
      val half = xs.size / 2
      xs.take(half).reverse == xs.takeRight(half)
    }
  }

  // P07 (**) Flatten a nested list structure.
  //     Example:
  //     scala> flatten(List(List(1, 1), 2, List(3, List(5, 8))))
  //     res0: List[Any] = List(1, 1, 2, 3, 5, 8)
  object P07 {

    def flatten(xs: List[_]): List[_] = {
      @tailrec
      def doFlatten(xss: List[_], acc: List[_]): List[_] =
        xss match {
          case Nil => acc.reverse
          case h :: t =>
            h match {
              case hs: List[_] => doFlatten(hs ::: t, acc)
              case _ => doFlatten(t, h :: acc)
            }
        }

      doFlatten(xs, Nil)
    }
  }

  // P08 (**) Eliminate consecutive duplicates of list elements.
  //     If a list contains repeated elements they should be replaced with a
  //     single copy of the element.  The order of the elements should not be
  //     changed.
  //
  //     Example:
  //     scala> compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
  //     res0: List[Symbol] = List('a, 'b, 'c, 'a, 'd, 'e)
  object P08 {

    def compress[E](xs: List[E]): List[E] = {
      xs match {
        case Nil => xs
        case _ =>
          val (ys, _) =
            xs.foldLeft((xs.head :: Nil, xs.head)) {
              case ((acc, prevElem), e) =>
                prevElem == e match {
                  case true => acc -> e
                  case false => (e :: acc) -> e
                }
            }
          ys.reverse
      }
    }

    def compressRecursive[E](xs: List[E]): List[E] = {
      @tailrec
      def doCompress(xss: List[E], acc: List[E]): List[E] =
        (xss, acc) match {
          case (Nil, _) => acc.reverse
          case (h :: t, Nil) => doCompress(t, h :: Nil)
          case (h :: t, hh :: tt) =>
            h == hh match {
              case true => doCompress(t, acc)
              case false => doCompress(t, h :: acc)
            }
        }

      doCompress(xs, Nil)
    }
  }

}