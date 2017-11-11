package p1to10

object P01to10 {
  def encode[A](list: List[A]):List[(Int, A)] = ???

  def pack[A](list: List[A]): List[List[A]] = ???

  def compress[A](list: List[A]): List[A] = ???

  def flatten[A, B](list: List[A]): List[B] = ???

  def isPalindrome[A](list: List[A]): Boolean = ???

  def reverse[A](list: List[A]): List[A] = ???

  def length[A](list: List[A]): Int = ???


  def last[A](list: List[A]): A ={
    //list.last
    list match {
      case h :: Nil => h //要素が１つ
      case _ :: subList => last(subList)  //再帰的に探索していく
      case _ => throw new NoSuchElementException //ここは来るのか？
    }
  }

  //Find the last but one element of a list.
  def penultimate[A](list: List[A]): A ={
    list match {
      case a :: Nil => a
      case a :: _ :: Nil => a
      case _ :: b :: restList => penultimate( b :: restList)
      case _  => throw new NoSuchElementException
    }
  }

  //By convention, the first element in the list is element 0.
  def nth[A](i: Int, list: List[A]): A = {
    //TODO これだとマイナス対応ができていない
    (i, list) match {
      case (0, a :: Nil) => a
      case (n, _ :: subList) => nth(n - 1, subList)
      case (_, Nil) => throw new NoSuchElementException
    }
  }
}
