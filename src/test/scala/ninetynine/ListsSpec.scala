package ninetynine

import org.specs2.mutable.Specification

class ListsSpec extends Specification {

  sequential

  "Last element in List(1, 1, 2, 3, 5, 8)" should {
    "be 8" >> {
      Lists.P01.last(List(1, 1, 2, 3, 5, 8)) ==== 8
    }
  }

  "Penultimate element in List(1, 1, 2, 3, 5, 8)" should {
    "be 5" >> {
      Lists.P02.penultimate(List(1, 1, 2, 3, 5, 8)) ==== 5
      Lists.P02.penultimateBuiltIn(List(1, 1, 2, 3, 5, 8)) ==== 5
    }
  }

  "Second element in List(1, 1, 2, 3, 5, 8)" should {
    "be 2" >> {
      Lists.P03.nth(2, List(1, 1, 2, 3, 5, 8)) ==== 2
      Lists.P03.nthBuiltIn(2, List(1, 1, 2, 3, 5, 8)) ==== 2
    }
  }

  "Length of List(1, 1, 2, 3, 5, 8)" should {
    "be 6" >> {
      Lists.P04.length(List(1, 1, 2, 3, 5, 8)) ==== 6
      Lists.P04.lengthBuiltIn(List(1, 1, 2, 3, 5, 8)) ==== 6
    }
  }

  "Reverse ordering of List(1, 1, 2, 3, 5, 8)" should {
    "be List(8, 5, 3, 2, 1, 1)" >> {
      Lists.P05.reverse(List(1, 1, 2, 3, 5, 8)) ==== List(8, 5, 3, 2, 1, 1)
      Lists.P05.reverseBuiltIn(List(1, 1, 2, 3, 5, 8)) ====
        List(8, 5, 3, 2, 1, 1)
    }
  }

  "List(1, 2, 3, 2, 1)" should {
    "be a palindrome" >> {
      Lists.P06.isPalindrome(List(1, 2, 3, 2, 1)) ==== true
      Lists.P06.isPalindromeFaster(List(1, 2, 3, 2, 1)) ==== true
    }
  }

  "Flattening List(List(1, 1), 2, List(3, List(5, 8)))" should {
    "yield List(1, 1, 2, 3, 5, 8)" >> {
      Lists.P07.flatten(List(List(1, 1), 2, List(3, List(5, 8)))) ===
        List(1, 1, 2, 3, 5, 8)
    }
  }

  "Compressing a list" should {
    "remove duplicate copies of repeating elements" >> {
      Lists.P08.compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) ====
        List('a, 'b, 'c, 'a, 'd, 'e)
      Lists.P08.compressRecursive(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) ====
        List('a, 'b, 'c, 'a, 'd, 'e)
    }
  }

  "Packing consecutive duplicates into sublists of " +
    "List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)" should {
    "yield List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e))" >> {
      Lists.P09.pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) ====
        List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e))
    }
  }

  "Run-length encoding of List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)" should {
    "yield List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))" >> {
      Lists.P10.encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) ====
        List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e))
    }
  }

  "Modified run-length encoding of List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)" should {
    "yield List((4,'a), 'b, (2,'c), (2,'a), 'd, (4,'e))" >> {
      Lists.P11.encodeModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) ===
        List((4, 'a), 'b, (2, 'c), (2, 'a), 'd, (4, 'e))
    }
  }

  "Decoding run-length encoded List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e))" should {
    "yield List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)" >> {
      Lists.P12.decode(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e))) ====
        List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
    }
  }
}