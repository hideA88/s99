package p01to28

import org.scalatest._

class P01to10Spec extends FlatSpec with Matchers {
  "P01" should "get list last" in {
    val list = List(1, 1, 2, 3, 5, 8)
    P01to10.last(list) shouldEqual 8
  }

  "P02" should "Find the last but one element of a list." in {
    val list = List(1, 1, 2, 3, 5, 8)
    P01to10.penultimate(list) shouldEqual 5
  }

  "P03" should "Find the Kth element of a list." in {
    val list = List(1, 1, 2, 3, 5, 8)
    P01to10.nth(2, list) shouldEqual 2
  }

  "P04" should "Find the number of elements of a list." in {
    val list = List(1, 1, 2, 3, 5, 8)
    P01to10.length(list) shouldEqual 6
  }

  "P05" should "Reverse a list." in {
    val list = List(1, 1, 2, 3, 5, 8)
    P01to10.reverse(list) shouldEqual List(8, 5, 3, 2, 1, 1)
  }

  "P06" should "Find out whether a list is a palindrome." in {
    val list = List(1, 2, 3, 2, 1)
    P01to10.isPalindrome(list) shouldEqual true
  }

  "P07" should "Flatten a nested list structure." in {
    val list = List(List(1, 1), 2, List(3, List(5, 8)))
    P01to10.flatten(list) shouldEqual  List(1, 1, 2, 3, 5, 8)
  }

  "P08" should "Eliminate consecutive duplicates of list elements." in {
    val list = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
    P01to10.compress(list) shouldEqual  List('a, 'b, 'c, 'a, 'd, 'e)
  }

  "P09" should "Pack consecutive duplicates of list elements into sublists." in {
    val list = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
    P01to10.pack(list) shouldEqual List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e))
  }

  "P10" should "Run-length encoding of a list." in {
    val list = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
    P01to10.encode(list) shouldEqual List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))
  }
}
