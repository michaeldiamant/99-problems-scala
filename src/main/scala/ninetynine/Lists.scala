package ninetynine

import scala.annotation.tailrec

object Lists {

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

  object P06 {

    def isPalindrome[E](xs: List[E]): Boolean =
      xs == xs.reverse

    def isPalindromeFaster[E](xs: List[E]): Boolean = {
      // Ignore middle element when size is odd
      val half = xs.size / 2
      xs.take(half).reverse == xs.takeRight(half)
    }
  }

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

  object P09 {

    def pack(xs: List[Symbol]): List[List[Symbol]] =
      xs.foldLeft(List.empty[List[Symbol]]) {
        (acc, s) =>
          acc match {
            case Nil => (s :: Nil) :: acc
            case h :: t =>
              // h can never be empty.  Ideally is represented by
              // scalaz.NonEmptyList
              h.head == s match {
                case true => (s :: h) :: t
                case false => (s :: Nil) :: h :: t
              }
          }
      }
        .reverse
  }

  object P10 {

    def encode(xs: List[Symbol]): List[(Int, Symbol)] =
      for (symbols <- P09.pack(xs)) yield
        symbols.size -> symbols.head
  }

  object P11 {

    def encodeModified(xs: List[Symbol]): List[Any] =
      for (t@(size, elem) <- P10.encode(xs)) yield
        size == 1 match {
          case true => elem
          case false => t
        }

  }

  object P12 {

    def decode(xs: List[(Int, Symbol)]): List[Symbol] =
      for {
        (size, elem) <- xs
        elems <- List.fill(size)(elem)
      } yield
        elems
  }

  object P13 {

    def encodeDirect(xs: List[Symbol]): List[(Int, Symbol)] =
      xs.foldLeft(List.empty[(Int, Symbol)]) {
        (acc, s) =>
          acc match {
            case Nil => 1 -> s :: acc
            case (count, ss) :: t =>
              // h can never be empty.  Ideally is represented by
              // scalaz.NonEmptyList
              ss == s match {
                case true => count + 1 -> s :: t
                case false => 1 -> s :: count -> ss :: t
              }
          }
      }
        .reverse
  }

  object P14 {

    def duplicate[E](xs: List[E]): List[E] =
      for {
        e <- xs
        ee <- e :: e :: Nil
      } yield
        ee
  }

  object P15 {

    def duplicateN[E](n: Int, xs: List[E]): List[E] =
      for {
        e <- xs
        ee <- List.fill(n)(e)
      } yield
        ee
  }

  object P16 {

    def drop[E](nth: Int, xs: List[E]): List[E] =
      for {
        (e, i) <- xs.zipWithIndex
        if (i + 1) % nth != 0
      } yield
        e
  }

  object P17 {

    def split[E](n: Int, xs: List[E]): (List[E], List[E]) = {
      @tailrec
      def doSplit(xss: List[E], acc: List[E]): (List[E], List[E]) =
        xss match {
          case Nil => acc.reverse -> Nil
          case h :: t if acc.size < n => doSplit(t, h :: acc)
          case _ => acc.reverse -> xss

        }

      doSplit(xs, Nil)
    }
  }

  object P18 {

    def slice[E](i: Int, j: Int, xs: List[E]): List[E] = {
      @tailrec
      def doSlice(xss: List[E], acc: List[E], elementsDiscarded: Int): List[E] =
        elementsDiscarded >= i match {
          case true =>
            elementsDiscarded < j match {
              case true =>
                xss match {
                  case Nil => acc.reverse
                  case h :: t => doSlice(t, h :: acc, elementsDiscarded + 1)
                }
              case false => acc.reverse
            }
          case false => doSlice(xss.tail, acc, elementsDiscarded + 1)
        }

      doSlice(xs, Nil, 0)
    }
  }

  object P19 {

    def rotate[E](shift: Int, xs: List[E]): List[E] = {
      @tailrec
      def doRotate(xss: List[E], acc: List[E]): List[E] =
        (xss, acc.size < shift.abs) match {
          case (Nil, _) =>
            shift > 0 match {
              case true => xss ::: acc.reverse
              case false => acc ::: xss.reverse
            }
          case (h :: t, true) => doRotate(t, h :: acc)
          case (_, false) =>
            shift > 0 match {
              case true => xss ::: acc.reverse
              case false => acc ::: xss.reverse
            }
        }

      doRotate(
        shift > 0 match {
          case true => xs
          case false => xs.reverse
        },
        Nil)
    }
  }

  object P20 {

    def removeAt[E](i: Int, xs: List[E]): (List[E], E) = {
      @tailrec
      def doRemoveAt(xss: List[E], acc: List[E]): (List[E], E) =
        (acc.size == i, xss) match {
          // Ignoring case where list is smaller than i
          // i.e. case (_, Nil) =>
          case (_, Nil) => sys.error("Unsupported use case")
          case (true, h :: t) => (acc.reverse ::: t) -> h
          case (false, h :: t) => doRemoveAt(t, h :: acc)
        }

      doRemoveAt(xs, Nil)
    }

  }

}