trait MyEither[+E, +A] {
  def map[B](f: A => B): MyEither[E, B]


  def flatMap[EE >: E, B](f: A => MyEither[EE, B]): MyEither[EE, B]

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B]

  //
  //  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C]
}

case class MyLeft[+E](value: E) extends MyEither[E, Nothing] {
  def map[B](f: Nothing => B): MyEither[E, B] = MyLeft(value)

  def flatMap[EE >: E, B](f: Nothing => MyEither[EE, B]): MyEither[EE, B] = MyLeft(value)

  def orElse[EE >: E, B >: Nothing](b: => Either[EE, B]): Either[EE, B] = b

}

case class MyRight[+A](value: A) extends MyEither[Nothing, A] {
  def map[B](f: A => B): MyEither[Nothing, B] = MyRight(f(value))

  def flatMap[EE >: Nothing, B](f: A => MyEither[EE, B]): MyEither[EE, B] = f(value)

  def orElse[EE >: Nothing, B >: A](b: => Either[EE, B]): Either[EE, B] = MyRight(value)
}

