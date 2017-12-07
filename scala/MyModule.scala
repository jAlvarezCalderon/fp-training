object MyModule {

// Cosas para tener en cuenta en recursion
// 1 . Identificar el iterador o sobre que se va basar el iterador
// 2 . identificar cual sera el estado de la iteracion
// 3 . identifica el algoritmo que soluciona el problema
	def factorial(x:Int):Int = {	
		@annotation.tailrec
		def loop(x:Int, acc:Int):Int = {
			if(x <= 0) acc
			else loop(x-1, x*acc)
		}
		loop(x,1)
	}

	def abs(n:Int):Int =
		if(n < 0) -n
		else n

// Esta es un funcion de alto orden dado que puede recibir funcoines como parametros
// y asi mismo devolver funcoines como su dato de salida
	def formatResult(name:String, n:Int, f: Int => Int): String = {
		val msg = "The %s of %d is %d"
		msg.format( name, n, f(n) )
	}

// Parametric pollimorfic fucntion: Son aquellas que su datos de entrada son de un tipo Generic tal como A,B,C.	
// Monomorphick fucntions: son las que tiene sus tipos hasrdcoded tales como String Int
	def main (args:Array[String]): Unit =
		println(formatResult("abs", -47, abs ))
		println(formatResult("factorial", 7, factorial ))

	def findFirts[A](as:Array[A], p:A=>Boolean) : Int = {
		@annotation.tailrec
		def loop(n:Int): Int = 
			if(n >= as.length) -1
			else if (p(as(n))) n
			else loop(n + 1)

		loop(0)
	}

	def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
		@annotation.tailrec
		def loop(n:Int, s:Boolean ):Boolean =
			if(n >= as.length-1) s
			else if (ordered(as(n), as(n+1)) == false) false
			else loop(n+1,ordered(as(n), as(n+1)))
		loop(0, true) // inicio recursion
	}

//	def curry[A,B,C](f: (A, B) => C): A => (B => C) = 
//		(a: A) => ( b:B => f( a, b ) )

//	def uncurry[A,B,C](f: A => B => C): (A, B) => C
		//(a:A, b:B) => f(a)(b)

	//def compose[A,B,C](f: B => C, g: A => B): A => C
	//	(a:A) => f(g(a))

	def getIndex[A](s:String): Int = {
		6
	}

	def tupla(a:Int, s:Array[Int]):(Int,Int) = {
		def loop(a:Int, b:Int): (Int,Int) =
			if(b == s.length-1) (a,b)
			if((s(a) + s(b)) == 100 ) (a,b) 
			else loop(a,b+1)
		loop(0,1)
	}



}

