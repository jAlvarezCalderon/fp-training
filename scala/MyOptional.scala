trait MyOptional[+A] {
  def filter(f: A => Boolean): MyOptional[A]

  def flatMap[B](f: A => MyOptional[B]): MyOptional[B]

  def orElse[B >: A](default: => MyOptional[B]): MyOptional[B]

  def map[B](f: A => B): MyOptional[B]

  def getOrElse[B >: A](default: => B): B


}

case object Nones extends MyOptional[Nothing] {
  def filter(f: Nothing => Boolean): MyOptional[Nothing] = Nones

  def flatMap[B](f: Nothing => MyOptional[B]): MyOptional[B] = Nones

  def orElse[B >: Nothing](default: => MyOptional[B]): MyOptional[B] = default

  def map[B](f: Nothing => B): MyOptional[B] = Nones

  def getOrElse[B >: Nothing](default: => B): B = default
}

case class Something[A](value: A) extends MyOptional[A] {
  def filter(f: A => Boolean): MyOptional[A] = if (f(value)) Something(value) else Nones

  def flatMap[B](f: A => MyOptional[B]): MyOptional[B] = f(value)

  def orElse[B >: A](default: => MyOptional[B]): MyOptional[B] = Something(value)

  def map[B](f: A => B): MyOptional[B] = Something(f(value))

  def getOrElse[B >: A](default: => B): B = value

}


object MyOptional {

  def Try[A](a: => A): MyOptional[A] =
    try Something(a)
    catch {
      case e: Exception => Nones
    }

  def variance(xs: Seq[Double]): MyOptional[Double] =
    mean(xs) flatMap (m => mean(xs.map(x => math.pow(x - m, 2))))


  def mean(xs: Seq[Double]): MyOptional[Double] =
    if (xs.isEmpty) Nones
    else Something(xs.sum / xs.length)

  def parseInsuranceRateQuote(age: String, numberOfSpeedingTickets: String): MyOptional[Double] = {
    val optAge: MyOptional[Int] = Try {
      age.toInt
    }
    val optTickets: MyOptional[Int] = Try {
      numberOfSpeedingTickets.toInt
    }
    map2(optAge, optTickets)(insuranceRateQuote)
  }

  def map2[A, B, C](a: MyOptional[A], b: MyOptional[B])(f: (A, B) => C): MyOptional[C] =
    a.flatMap(a => b.map(b => f(a, b)))

  def insuranceRateQuote(age: Int, numberOfSpeedingTickets: Int): Double = (age / numberOfSpeedingTickets) * 1.0

  def sequence[A](a: List[MyOptional[A]]): MyOptional[List[A]] = a match {
    case Nil => Something(Nil)
    case h :: t => h.flatMap(hh => sequence(t).map(tt => hh :: tt))
  }

  def traverse[A, B](a: List[A])(f: A => MyOptional[B]): MyOptional[B] = a match {
    case None => Some(None)
    case h :: t => map2(f(h), traverse(t)(f))(_ :: _)
}



}
