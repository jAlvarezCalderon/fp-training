sealed trait Listo[+A]
case object Nil extends Listo[Nothing]
case class Cons[+A](head:A, tail:Listo[A]) extends Listo[A]

object Listo {

	def sum(ints: Listo[Int]):Int = ints match {
		case Nil => 0
		case Cons(x,xs) => x + sum(xs)
	}

	def product(doubles: Listo[Double]): Double = doubles match {
		case Nil => 0.0
		case Cons(0.0,_) => 0.0
		case Cons(x,xs) => x * product(xs) 
	}

	def apply[A](as: A*): Listo[A] = 
		if(as.isEmpty) Nil
		else Cons(as.head, apply(as.tail:_*))

	val x = Listo(4,5,6,1,2,3) match {
		case Cons(x,Cons(2,Cons(4,_))) => x
		case Nil => 42
		case Cons(x,Cons(y,Cons(6,Cons(1,_)))) => x + y
		case Cons(h, t) => h + sum(t)
		case _ =>  101
	}

	def tail[A](xs:Listo[A]):Listo[A] = xs match {
		case Nil => xs
		case Cons(x,xs) => xs
	}

	def setHead[A](h:A,xs:Listo[A]):Listo[A] = h match {
		case Nil => xs
		case x => Cons(x, xs) 
	}

	def drop[A](n:Int,xs:Listo[A]):Listo[A] = n match {
		case 0 => xs
		case n => drop(n-1, tail(xs))
	}

	def dropWhile[A](xs:Listo[A])(f: A => Boolean):Listo[A] = xs match {
		case Cons(h,t) if f(h) => dropWhile(t)(f)
		case _ => xs
	}

	def sum2(xs: Listo[Int]) = foldLeft(xs,0)(_+_)

	def product2(xs: Listo[Int]) = foldLeft(xs, 1)(_*_) 
	
	def length[A](xs:Listo[A]): Int = foldLeft(xs, 0)((_,z) => z + 1)

    def init[A](xs:Listo[A]): Listo[A] = xs match {
    	case Nil => Nil
    	case Cons(_,Nil) => Nil
    	case Cons(x,xs) => Cons(x, init(xs))
    }

	def foldRight[A,B](xs: Listo[A], z:B)(f: (A,B) => B): B = xs match {
		case Nil => z
		case Cons(x,xs) => f(x, foldRight(xs,z)(f))
	}

	def foldLeft[A,B](xs: Listo[A], z: B)(f: (A, B) => B): B = xs match {
		case Nil => z
		case Cons(x,xs) => foldLeft(xs, f(x,z))(f)
	}

	def reverse[A](xs:Listo[A]):Listo[A] = xs match {
		case Nil => xs
		case Cons(x,xs) => foldLeft(xs,Listo(x))((x,b) => Cons(x,b))
	}

	def append[A](as:Listo[A], ys:Listo[A]):Listo[A] = foldRight(as,ys)((x,y) => Cons(x,y))

	def concatenate[A](xs: Listo[Listo[A]]): Listo[A] = xs match {
		case Nil => Nil
		case Cons(x, xs) => append(x, concatenate(xs))  
	}

	def transform[A,B](xs:Listo[A], ys:Listo[B])(f: A => B) :Listo[B] = xs match {
		case Nil => ys
		case Cons(x,xs) => transform(xs,Cons(f(x),ys))(f)
	}

	def map[A,B](as: Listo[A])(f: A => B): Listo[B] = as match {
		case Nil => Nil
		case Cons(x,xs) => Cons(f(x),map(xs)(f))
	}

	def filter[A](as: Listo[A])(f: A => Boolean): Listo[A] =  foldRight(as, Nil:Listo[A])((h,t) => if (f(h)) Cons(h,t) else t)

	def flatMap[A,B](as: Listo[A])(f: A => Listo[B]): Listo[B] = concatenate(map(as)(f))

	def filter2[A,B](as:Listo[A])(f: A => Boolean): Listo[B] = flatMap(as)(x => if (f(x)) Listo(x) else Nil)

	def zipWith[A,B,C](xs:Listo[A], ys:Listo[B])(f : (A,B) => C ) : Listo[C] = (xs, ys) match {
		case (Nil,_) => Nil
		case (_,Nil) => Nil
		case (Cons(h1,t1),Cons(h2,t2)) => Cons(f(h1,h2), zipWith(t1,t2)(f))
	}

	def hasSubSequent[A](xs:Listo[A]): Boolean = xs match {
		case Nil => false
		case Cons(x,Nil) => false
		case Cons(y,Cons(a,as)) => true
	}





}