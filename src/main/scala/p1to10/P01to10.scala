package p1to10

import scala.annotation.tailrec

object P01to10 {
  def encode[A](list: List[A]):List[(Int, A)] = {
    //TODO 走査が２回走っているのでリファクタしたほうがいい
    pack(list).map(el => (el.size, el.head))
  }

  def pack[A](list: List[A]): List[List[A]] = {
    val init    : List[A]       = Nil
    val accList : List[List[A]] = Nil

    val (ac, lastGroup) = list.foldLeft((accList, init)){case((acc, accGroup), elem) =>
      accGroup.headOption match {
        case Some(a) if a == elem => (acc, a :: accGroup)
        case Some(_)              => (accGroup :: acc, elem :: Nil)
        case None                 => (acc, elem :: Nil)
      }
    }
    (lastGroup :: ac).reverse
  }


  def compress[A](list: List[A]): List[A] = {
    val init : List[A] = Nil
    list.foldLeft(init){case(acc, elem) =>
      acc.headOption match {
        case Some(a) if a == elem => acc
        case _                    => elem :: acc
      }
    }.reverse
  }

  def flatten(list: List[Any]): List[Any] = {
    _flatten(list).reverse
  }

  //TODO tailrec
  private def _flatten(list: List[Any]): List[Any] = {
    val init : List[Any] = Nil
    list.foldLeft(init){ case (acc, elem) =>
      elem match {
        case a @ List(_*) => _flatten(a) ::: acc
        case b            => b :: acc
      }
    }
  }


  def isPalindrome[A](list: List[A]): Boolean = {
    val reverseList = list.reverse
    reverseList == list
  }

  def reverse[A](list: List[A]): List[A] = {
    list.reverse
  }

  def length[A](list: List[A]): Int = {
    list.size
  }


  def last[A](list: List[A]): A ={
    //list.last
    list match {
      case h :: Nil     => h //要素が１つ
      case _ :: subList => last(subList)  //再帰的に探索していく
      case _            => throw new NoSuchElementException //ここは来るのか？
    }
  }

  //Find the last but one element of a list.
  def penultimate[A](list: List[A]): A ={
    list match {
      case a :: Nil           => a
      case a :: _ :: Nil      => a
      case _ :: b :: restList => penultimate( b :: restList)
      case _                  => throw new NoSuchElementException
    }
  }

  //By convention, the first element in the list is element 0.
  def nth[A](i: Int, list: List[A]): A = {
    //TODO これだとマイナス対応ができていない
    (i, list) match {
      case (0, a :: _)       => a
      case (n, _ :: subList) => nth(n - 1, subList)
      case (_, Nil)          => throw new NoSuchElementException
    }
  }
}
