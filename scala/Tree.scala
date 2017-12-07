sealed trait MyTree[+A]
case class Leaf[A](value: A) extends MyTree[A]
case class Branch[A](left: MyTree[A], right: MyTree[A]) extends MyTree[A]

object MyTree {

	def size[A](t:MyTree[A]):Int = t match {
		case Leaf => 1
		case Branch(l,r) => 1 + size(l) + size(r) 
	}	
}

	//3.26
	//3.27
	//3.28
	//3.29