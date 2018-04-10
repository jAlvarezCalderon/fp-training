

sealed trait MyStream[+A] {
  def toList: List[A]

  def take(n: Int): MyStream[A]

  def drop(n: Int): MyStream[A]

  def takeWhile(p: A => Boolean): MyStream[A]
}

case object Empty extends MyStream[Nothing] {
  def toList: List[Nothing] = List()

  def take(n: Int): MyStream[Nothing] = Empty

  def drop(n: Int): MyStream[Nothing] = Empty

  def takeWhile(p: Nothing => Boolean): MyStream[Nothing] = Empty
}

case class Cons[+A](h: () => A, t: () => MyStream[A]) extends MyStream[A] {
  def toList: List[A] = {
    def loop(s: MyStream[A], nl: List[A]): List[A] = s match {
      case Empty => nl
      case Cons(h, t) => loop(t(), nl ::: List(h()))
    }

    loop(this, List())
  }

  def take(n: Int): MyStream[A] = {
    def loop(index: Int, os: MyStream[A], ns: MyStream[A]): MyStream[A] = os match {
      case Empty => ns
      case Cons(h, t) => if (index >= n) ns else Cons(h, () => loop(index + 1, t(), ns))
    }

    loop(0, this, Empty)
  }

  def drop(n: Int): MyStream[A] = {
    def loop(index: Int, os: MyStream[A]): MyStream[A] = os match {
      case Empty => Empty
      case Cons(h, t) => if (index >= n) t() else loop(index + 1, t())
    }

    loop(1, this)
  }

  def takeWhile(p: A => Boolean): MyStream[A] = {
    def loop(os: MyStream[A], ns: MyStream[A]): MyStream[A] = os match {
      case Empty => ns
      case Cons(h, t) => if (p(h())) Cons(h, () => loop(t(), ns)) else ns
    }
    loop(this, Empty)
  }
}


object MyStream {
  def cons[A](hd: => A, tl: => MyStream[A]): MyStream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: MyStream[A] = Empty

  def apply[A](as: A*): MyStream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

}