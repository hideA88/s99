package p01to28

import org.scalatest._

class P21to28Spec extends FlatSpec with Matchers {
  "P21" should "Insert an element at a given position into a list." in {
    val list = List('a, 'b, 'c, 'd)
    P21to28.insertAt('new, 1, list) shouldEqual  List('a, 'new, 'b, 'c, 'd)
  }

  "P22" should "Create a list containing all integers within a given range." in {
    P21to28.range(4, 9) shouldEqual List(4, 5, 6, 7, 8, 9)
  }

  "P23" should "Extract a given number of randomly selected elements from a list." in {
    val list = List('a, 'b, 'c, 'd, 'f, 'g, 'h)
    P21to28.randomSelect(3, list) shouldEqual  List('e, 'd, 'a)
  }

  "P24" should "Lotto: Draw N different random numbers from the set 1..M." in {
    P21to28.lotto(6, 49) shouldEqual List(23, 1, 17, 33, 21, 37)
  }

  "P25" should "Generate a random permutation of the elements of a list." in {
    val list = List('a, 'b, 'c, 'd, 'e, 'f)
    P21to28.randomPermute(list) shouldEqual List('b, 'a, 'd, 'c, 'e, 'f)
  }

  "P26" should "Generate the combinations of K distinct objects chosen from the N elements of a list." in {
    val list = List('a, 'b, 'c, 'd, 'e, 'f)
    P21to28.combinations(3, list) shouldEqual List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k)
  }

  "P27" should "Group the elements of a set into disjoint subsets." in {
    val list = List("Aldo", "Beat", "Carla", "David", "Evi", "Flip", "Gary", "Hugo", "Ida")
    P21to28.group3(list) shouldEqual (List('a, 'b, 'c),List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
  }

  "P28" should "Sorting a list of lists according to length of sublists." in {
    val list = List(List('a, 'b, 'c), List('d, 'e), List('f, 'g, 'h), List('d, 'e), List('i, 'j, 'k, 'l), List('m, 'n), List('o))
    P21to28.lsort(3, 7, list) shouldEqual List('d, 'e, 'f, 'g)
  }
}
