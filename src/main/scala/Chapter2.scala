object Ch2 {
  def fib(n: Int): List[Int] = {
    @annotation.tailrec
    def fib(n: Int, acc1: Int, acc2:Int, acc: List[Int]): List[Int] = {
      n match {
        case 0 => acc  
        case _ => fib(n-1, acc2, acc2 + acc1, Cons(acc1, acc))
      }
    }
    List.reverse(fib(n,0,1,List()))
  }

  def fibNumAt(n: Int): Int = {
    @annotation.tailrec
    def fib(n:Int, acc1: Int, acc2: Int): Int = {
      if(n < 0) 0
      else n match {
        case 0 => acc1
        case _ => fib(n-1, acc2, acc2 + acc1) 
      }
    }
    fib(n,0,1)   
  }

  @annotation.tailrec
  def isSorted[A](as: Array[A], ordered:(A,A) => Boolean): Boolean = {
    as match {
      case Array() => true
      case Array(a) => true
      case Array(a,b,_*) => if(!ordered(a,b)) false else isSorted(as.tail, ordered)
    }
  }

  def partial1[A,B,C](a: A, f: (A,B) => C): B => C = {
    (b: B) => f(a, b) 
  }
  
  def add(a:Int ,b: Int) = a + b

  val plus1 = partial1(1, add)

  def curry[A,B,C](f: (A,B) => C ): A => (B => C) = {
    (a:A) => (b:B) => f(a, b)
  }

  val addCurried = curry(add)
  val plus1v2 = addCurried(1)

  def uncurry[A,B,C](f:(A) => (B => C)): (A,B) => C = {
    (a:A, b:B) => f(a)(b)
  }

  val originalAdd = uncurry(addCurried)

  def compose[A,B,C](f: B => C, g: A => B): A => C = {
    (a: A) => f(g(a))
  }

  def double(x: Int) = x * 2

  val comp = compose(double, plus1)

  def main(args: Array[String]):Unit = {
    println(fib(10))
    println(isSorted(Array(1,2,3),(a:Int,b:Int) => a < b))
    println(isSorted(Array(1), (a:Int, b:Int) => a < b))
    println(isSorted(Array(3,2,4,5), (a:Int,b:Int) => a < b)) 
    println(plus1(2))  
    println(plus1v2(2))
    println(originalAdd(1,2))
    println(comp(3))
    println(fibNumAt(10))
    println(fibNumAt(1))
    println(fibNumAt(0))
  }
}

