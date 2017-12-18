sealed trait Optionales[+A] {
  def map[B](f: A => B): Optionales[B]
  def flatMap[B](f: A => Optionales[B]): Optionales[B]
  def getOrElse[B >: A](default: => B) :B
  def orElse[B >: A](on: => Optionales[B]): Optionales[B]
  def filter(f: A => Boolean) :Optionales[A]
}

case object Nones extends Optionales[Nothing] {
  def map[B](f: Nothing => B): Optionales[B] = Nones
  def flatMap[B](f: Nothing => Optionales[B]): Optionales[B] = Nones
  def getOrElse[B >: Nothing](default: => B) :B = default
  def orElse[B >: Nothing](on: => Optionales[B]): Optionales[B] = on
  def filter(f: Nothing => Boolean) :Optionales[Nothing] = Nones
}

case class Something[+A](value: A) extends Optionales[A] {
  def map[B](f: A => B): Optionales[B] = Something(f(value))
  def flatMap[B](f: A => Optionales[B]): Optionales[B] = f(value)
  def getOrElse[B >: A](default: => B) :B = value
  def orElse[B >: A](on: => Optionales[B]): Optionales[B] = this
  def filter(f: A => Boolean) :Optionales[A] = if(f(value)) this else Nones
}

object Optionales {

  def mean(xs: Seq[Double]): Optionales[Double] =
    if(xs.isEmpty) Nones
    else Something(xs.sum/xs.length)

  def variance(xs: Seq[Double]): Optionales[Double] =
    mean(xs).flatMap((x) => mean(xs.map((l)=> math.pow(l - x, 2))))
}

