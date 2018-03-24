package p01to28

import org.scalatest._

class P11to20Spec extends FlatSpec with Matchers {
  "P11" should "Modified run-length encoding." in {
    val list = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
    P11to20.encodeModified(list) shouldEqual List((4,'a), 'b, (2,'c), (2,'a), 'd, (4,'e))
  }

  "P12" should "Decode a run-length encoded list." in {
    val list = List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e))
    P11to20.decode(list) shouldEqual List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
  }

  "P13" should "Run-length encoding of a list (direct solution)." in {
    val list = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
    P11to20.encodeDirect(list) shouldEqual List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))
  }

  "P14" should "Duplicate the elements of a list." in {
    val list = List('a, 'b, 'c, 'c, 'd)
    P11to20.duplicate(list) shouldEqual List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd)
  }

  "P15" should ".Duplicate the elements of a list a given number of times." in {
    val list = List('a, 'b, 'c, 'c, 'd)
    P11to20.duplicateN(3, list) shouldEqual List('a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c, 'c, 'c, 'c, 'd, 'd, 'd)
  }

  "P16" should "Drop every Nth element from a list." in {
    val list = List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)
    P11to20.drop(3, list) shouldEqual List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k)
  }

  "P17" should "Split a list into two parts." in {
    val list = List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)
    P11to20.split(3, list) shouldEqual (List('a, 'b, 'c),List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
  }

  "P18" should "Extract a slice from a list." in {
    val list = List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)
    P11to20.slice(3, 7, list) shouldEqual List('d, 'e, 'f, 'g)
  }

  "P19" should "Rotate a list N places to the left." in {
    val list = List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)
    P11to20.rotate(3, list) shouldEqual List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'a, 'b, 'c)
    P11to20.rotate(-2, list) shouldEqual List('j, 'k, 'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i)
  }

  "P20" should "Remove the Kth element from a list." in {
    val list =  List('a, 'b, 'c, 'd)
    P11to20.removeAt(1, list) shouldEqual (List('a, 'c, 'd),'b)
  }
}
