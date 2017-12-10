sealed trait Optional[+A] {
case class Something[+A](get:A) extends Optional[A]
case object Nones extends Optional[Nothing]

def mean(xs: Seq[Double]): Optional[Double] = 
	if(xs.isEmpty) Nones
	else Something(xs.sum / xs.length)

def map[B](f: A => B):Optional[B] = this match {
	case Nones => Nones
	case Something(a) => Something(f(a))  
}

def flatMap[B](f: A => Optional[B]) : Optional[B] = this match {
	case Nones => Nones
	case Something(a) => f(a)  
}

def getOrElse[B >: A](default: => B): B = ???
def orElse[B >: A](default: => B): B  = ??? 
def filter(f: A => Boolean) : Optional[A] = ???
}



