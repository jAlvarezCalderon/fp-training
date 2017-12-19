sealed trait Optionales[+A] {
  def map[B](f: A => B): Optionales[B]

  def flatMap[B](f: A => Optionales[B]): Optionales[B]

  def getOrElse[B >: A](default: => B): B

  def orElse[B >: A](on: => Optionales[B]): Optionales[B]

  def filter(f: A => Boolean): Optionales[A]
}

case object Nones extends Optionales[Nothing] {
  def map[B](f: Nothing => B): Optionales[B] = Nones

  def flatMap[B](f: Nothing => Optionales[B]): Optionales[B] = Nones

  def getOrElse[B >: Nothing](default: => B): B = default

  def orElse[B >: Nothing](on: => Optionales[B]): Optionales[B] = on

  def filter(f: Nothing => Boolean): Optionales[Nothing] = Nones
}

case class Something[+A](value: A) extends Optionales[A] {
  def map[B](f: A => B): Optionales[B] = Something(f(value))

  def flatMap[B](f: A => Optionales[B]): Optionales[B] = f(value)

  def getOrElse[B >: A](default: => B): B = value

  def orElse[B >: A](on: => Optionales[B]): Optionales[B] = this

  def filter(f: A => Boolean): Optionales[A] = if (f(value)) this else Nones
}

object Optionales {
  def mean(xs: Seq[Double]): Optionales[Double] =
    if (xs.isEmpty) Nones
    else Something(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Optionales[Double] =
    mean(xs).flatMap((x) => mean(xs.map((l) => math.pow(l - x, 2))))

  def lift[A, B](f: A => B): Optionales[A] => Optionales[B] = _ map f

  val abso: Optionales[Double] => Optionales[Double] = lift(math.abs)

  /**
    * Top secret formula for computing an annual car
    * insurance premium from two keys factors
    */
  def insuranceRateQuote(age: Int, numberOSpeedingTickets: Int): Double = age.toDouble / numberOSpeedingTickets.toDouble

  def parseInsuranceRateQuote(age: String, numberOfSpeedingTickets: String): Optionales[Double] = {
    val optAge: Optionales[Int] = Try(age.toInt)
    val optTickets: Optionales[Int] = Try(numberOfSpeedingTickets.toInt)
    map2(optAge, optTickets)(insuranceRateQuote)
  }

  def Try[A](a: => A): Optionales[A] =
    try Something(a)
    catch { case _: Exception => Nones }

  def map2[A, B, C](a: Optionales[A], b: Optionales[B])(f: (A, B) => C): Optionales[C] = a.flatMap(aa => b.map(bb => f(aa, bb)))

  def sequence[A](a: List[Optionales[A]]): Optionales[List[A]] = a match {
    case Nil => Nones
    case h :: t => h.flatMap(hh => sequence(t).map(tt => hh :: tt))
  }

  def parseInts(a: List[String]): Optionales[List[Int]] =
    sequence(a map (i => Try(i.toInt)))

  def traverse[A, B](a: List[A])(f: A => Optionales[B]): Optionales[List[B]] =
    a match {
      case Nil => Something(Nil)
      case h::t => map2(f(h), traverse(t)(f))(_ :: _)
    }
}

