package p01to28

object P11to20 {
  def removeAt[A](i: Int, list: List[A]): (List[A], A)  = {
    val init: (List[A], Option[A]) = (Nil, None)
    list.zipWithIndex.foldLeft(init){case((acc, v), (el, index)) =>
      if(i == index){
        (acc, Some(el))
      }else{
        (el :: acc, v)
      }
    } match {
      case (ac, Some(v)) => (ac.reverse, v)
      case (_, None)     => throw new Exception
    }
  }

  def rotate[A](i: Int, list: List[A]): List[A] = {
    val index = if(i > 0) i else i + list.size
    val (a, b)=list.splitAt(index)
    b ::: a
  }

  def slice[A](i: Int, j: Int, list: List[A]): List[A] = {
    list.slice(i, j)
  }

  def split[A](i: Int, list: List[A]):(List[A], List[A]) = {
    list.splitAt(i)
  }

  def drop[A](i: Int, list: List[A]): List[A] = {
    list.zipWithIndex.filter{case(_, index) => (index + 1) % i != 0}.map(_._1)
  }

  def duplicateN[A](i: Int, list: List[A]): List[A] = {
    list.flatMap(v => Seq.fill(i)(v))
  }

  def duplicate[A](list: List[A]): List[A]= {
    list.flatMap(v => Seq.fill(2)(v))
  }

  def encodeDirect[A](list: List[A]):List[(Int, A)] = {
    implicit val encoMonoid = new MonoidLike[List[(Int, A)]] {
      override def zero: List[(Int, A)] = Nil
      override def plus(a1: List[(Int, A)], a2: List[(Int, A)]): List[(Int, A)] = {
        (a1, a2.headOption) match {
          case ((i, a) :: rest, Some((j, el))) if a == el => (i + j, a) :: rest
          case (_, Some(_))                               => a2 ::: a1
          case (_, None)                                  => a1
          case _                                          => a2
        }
      }
      override def eq(a1: List[(Int, A)], a2: List[(Int, A)]): Boolean = {
        (a1.headOption, a2.headOption) match {
          case (Some((_, v1)), Some((_, v2))) if v1 == v2 => true
          case _ => false
        }
      }
    }

    basePack(list){el => List((1, el))}.reverse
  }


  def decode[A](list: List[(Int, A)]): List[A] = {
    list.flatMap({case(i, v) => Seq.fill(i)(v)})
  }

  def encodeModified[A](list: List[A]): List[Any] = {
    implicit val encoMonoid = new MonoidLike[List[Any]] {
      override def zero: List[Any] = Nil
      override def plus(a1: List[Any], a2: List[Any]): List[Any] = {
        (a1, a2.headOption) match {
          case ((i: Int, a) :: rest, Some((j:Int, el))) if a == el => (i + j, a) :: rest
          case (_, Some((1, el)))                                  => el :: a1
          case (_, Some(_))                                        => a2 ::: a1
          case (_, None)                                           => a1
          case _                                                   => a2
        }
      }
      override def eq(a1: List[Any], a2: List[Any]): Boolean = {
        (a1.headOption, a2.headOption) match {
          case (Some((_, v1)), Some((_, v2))) if v1 == v2 => true
          case _ => false
        }
      }
    }

    val f: A => List[Any] = el => List((1, el))
    basePack(list)(f).reverse
  }

  private def basePack[A, B](list: List[A])(convert: A => B)(implicit monoidB: MonoidLike[B]): B = {
    val (ac, lastGroup) = list.foldLeft((monoidB.zero, monoidB.zero)){case((acc, temp), elem) =>
      val v = convert(elem)
      temp match {
        case a if a == monoidB.zero   => (acc, v)
        case _ if monoidB.eq(v, temp) => (acc, monoidB.plus(temp, v))
        case _                        => (monoidB.plus(acc, temp), v)
      }
    }
    monoidB.plus(ac, lastGroup)
  }

  trait MonoidLike[A]{
    def zero: A
    def plus(a1: A, a2: A): A
    def eq(a1: A, a2: A): Boolean
  }
}
