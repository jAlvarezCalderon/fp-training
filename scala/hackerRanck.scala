def f(num: Int, arr: List[Int]): List[Int] = arr.map(a => replicate(a)(num))

def replicate[A](e: A)(t: Int): List[A] = {
  def loop(index: Int, nl: List[A]): List[A] = {
    if (index >= t) nl else loop(index + 1, e :: nl)
  }

  loop(0, List())
}

def lessThan3 = f(_)(x => x < 3)

def f(arr: List[Int])(predicate: Int => Boolean): List[Int] = {
  def loop(xs: List[Int], as: List[Int]): List[Int] = xs match {
    case Nil => as
    case l :: ls => if (predicate(l)) loop(ls, as ::: List(l)) else loop(ls, as)
  }

  loop(arr, List())
}

// https://www.hackerrank.com/challenges/fp-filter-positions-in-a-list/problem

def f(arr:List[Int]):List[Int] = filterBaseOnIndex(arr)(x => x % 2 == 0)

def filterBaseOnIndex(arr:List[Int])(cond: Int => Boolean): List[Int] = {
    def loop(index:Int, cl:List[Int], nl:List[Int]): List[Int] = cl match {
        case Nil => nl
        case h :: t => if(cond(index)) loop(index +1, t, nl ::: List(h) ) else loop(index +1, t, nl)
    }
    loop(1,arr,List())
}


