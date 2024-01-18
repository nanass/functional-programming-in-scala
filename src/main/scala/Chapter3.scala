// List
sealed trait List[+A] {

  def mkString[B](l: List[B]): String = {
    def appendComma(st: String): String = if (st.isEmpty) "" else ", "
    @annotation.tailrec
    def mkStringInternal(l: List[B], acc: String): String = l match {
      case Nil => acc
      case Cons(x, xs) => mkStringInternal(xs, s"$acc${appendComma(acc)}$x")
    }
    "(" + mkStringInternal(l, "") + ")"
  }

  override def toString = {
    mkString(this)
  }
}

case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  implicit def convertToList[A](a: Seq[A]): List[A] = {
    if (a.isEmpty) Nil
    else Cons(a.head, convertToList(a.tail))
  }

  def sum(ints: List[Int]): Int = {
    @annotation.tailrec
    def sum(ints: List[Int], acc: Int): Int = ints match {
      case Nil => acc
      case Cons(x, xs) => sum(xs, acc + x)
    }
    sum(ints, 0)
  }

  def product(ds: List[Double]): Double = {
    @annotation.tailrec
    def product(ds: List[Double], acc: Double): Double = ds match {
      case Nil => acc
      case Cons(0.0, _) => 0.0
      case Cons(x, xs) => product(xs, acc * x)
    }
    product(ds, 1.0)
  }


  // sum and product have many similarities, and can be abstracted
  // The common is the match and recursion over the list
  // the difference is the return when list is nil and the function to apply 
  // in the second parameter on recursion

  // place these differences in parameters
  // call the abstraction "foldRight"
  /*def foldRight[T, O](l: List[T], out: O)(fn: (T, O) => O) : O = l match {
  	case Nil => out
  	case Cons(x, xs) => fn(x, foldRight(xs, out)(fn))
  }*/

  def foldSum(l: List[Int]) = 
    foldRight(l, 0)(_ + _)
  
  def foldProduct(l: List[Double]) =
    foldRight(l, 1.0)(_ * _)

  /*
  * Can fold right calls be halted?
   *
   * It appears that if the fold function is not altered that it cannot halt execution. An exception can be thrown, but
   * then a value will not be returned. The fold function could be modified to halt execution with a guard function, that
   * would be applied and then return a specific value. This is the case because the fold function itself has control of
   * the execution path, rather than the function that is passed to the fold function.
   */

  /*
    EXERCISE 3.9
    Compute the length of a list using foldRight.
    def length[A](as: List[A]): Int
   */

  def length[A](l: List[A]): Int =
    foldRight(l, 0){
      (_, acc) => acc + 1
    }

  /*
    EXERCISE 3.10
    Our implementation of foldRight is not tail-recursive and will result in a StackOverflowError
    for large lists (we say it’s not stack-safe). Convince yourself that this is the
    case, and then write another general list-recursion function, foldLeft, that is
    tail-recursive, using the techniques we discussed in the previous chapter. Here is its
    signature: def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B
   */

  def foldLeft[A,B](as: List[A], z:B)(f: (B, A) => B): B =
    as match {
      case Nil => z
      case Cons(a, ar) => foldLeft(ar, f(z, a))(f)
    }
  /*
  EX. 11
  Write sum, product, and a function to compute the length of a list using foldLeft.
   */

  def foldLeftSum(l: List[Int]) = foldLeft(l, 0)(_ + _)
  def foldLeftProduct(l: List[Double]) = foldLeft(l, 0.0)(_ * _)
  def foldLeftLength[A](l: List[A]) = foldLeft(l, 0)((acc, _) => acc + 1)

/*
  EXERCISE 3.12
  Write a function that returns the reverse of a list (given List(1,2,3) it returns
    List(3,2,1)). See if you can write it using a fold.
    */

  def reverse[A](l: List[A]) : List[A] =
    foldLeft(l, Nil: List[A]){
      (acc, elem) =>
        Cons(elem, acc)
    }

/*
  EXERCISE 3.13
  Can you write foldLeft in terms of foldRight? How about the other way
    around? Implementing foldRight via foldLeft is useful because it lets us implement
  foldRight tail-recursively, which means it works even for large lists without overflowing
    the stack.
*/

  def foldRight[A, B](as: List[A], z: B)(f:(A, B) => B): B =
    foldLeft(as, (b:B) => b) {
      (acc, elem) =>
        (b1: B) => acc(f(elem, b1))
    }(z)

  def newFoldLeft[A, B](as: List[A], z: B)(f:(B, A) => B) : B =
    foldRight(as, (b: B) => b) {
      (elem, acc) =>
        (b1: B) => acc(f(b1, elem))
    }(z)

 /*
  EXERCISE 3.14
  Implement append in terms of either foldLeft or foldRight.
  */

  def append[A](l: List[A], elem: A) = foldRight(l, Cons(elem, Nil))(Cons(_,_))
  def append[A](l1: List[A], l2: List[A]) = foldRight(l1, l2)(Cons(_,_))

  /*
    EXERCISE 3.15
  Write a function that concatenates a list of lists into a single list. Its runtime
    should be linear in the total length of all lists. Try to use functions we have already
  defined.
  */

  def concat[A](lol : List[List[A]]): List[A] = foldRight(lol, Nil:List[A])(append)

/*
  EXERCISE 3.16
  Write a function that transforms a list of integers by adding 1 to each element.
  (Reminder: this should be a pure function that returns a new List!)
    */

  def add1(l:List[Int]): List[Int] = {
    foldRight(l,Nil:List[Int])((elem, list) => Cons(elem + 1, list))
  }

  /*
  EXERCISE 3.17
  Write a function that turns each value in a List[Double] into a String. You can use
  the expression d.toString to convert some d: Double to a String.
      */
  def doubleToString(l:List[Double]): List[String] = {
    foldRight(l, Nil:List[String])((elem, list) => Cons(elem.toString, list))
  }

  /*
  EXERCISE 3.18
  Write a function map that generalizes modifying each element in a list while maintaining
  the structure of the list. Here is its signature:12
  def map[A,B](as: List[A])(f: A => B): List[B]
      */

  def map[A,B](l: List[A])(fn: A => B): List[B] = {
    foldRight(l,Nil:List[B])((elem, list) => Cons(fn(elem), list))
  }

  /*
  EXERCISE 3.19
  Write a function filter that removes elements from a list unless they satisfy a given
  predicate. Use it to remove all odd numbers from a List[Int].
  def filter[A](as: List[A])(f: A => Boolean): List[A]
      */


  def filter[A](as: List[A])(f: A => Boolean): List[A] = {
    foldRight(as, Nil:List[A])((elem, list) => if(f(elem)) Cons(elem, list) else list)
  }

  /*
  EXERCISE 3.20
  Write a function flatMap that works like map except that the function given will return
  a list instead of a single result, and that list should be inserted into the final resulting
  list. Here is its signature:
  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B]
  For instance, flatMap(List(1,2,3))(i => List(i,i)) should result in
    List(1,1,2,2,3,3).
  */

  def flatMap[A,B](as: List[A])(f: A => List[B]) : List[B] = {
    foldRight(as,Nil:List[B])((elem, list) => append(f(elem),list))
  }

/*
  EXERCISE 3.21
  Use flatMap to implement filter.
  */
  def filter1[A](l:List[A])(fn: A => Boolean): List[A] = {
    flatMap(l)(e => if(fn(e)) List(e) else Nil)
  }

  /*
    EXERCISE 3.22
  Write a function that accepts two lists and constructs a new list by adding corresponding
    elements. For example, List(1,2,3) and List(4,5,6) become List(5,7,9).
    */

  def add(l1: List[Int], l2: List[Int]): List[Int] =  {
    def accumulate(l1: List[Int], l2: List[Int], acc: List[Int]): List[Int] = l1 match{
      case Cons(x1,xs1) => l2 match {
        case Cons(x2, xs2) => accumulate(xs1, xs2, Cons(x1 + x2, acc))
        case Nil => accumulate(xs1, Nil, Cons(x1, acc))
      }
      case Nil => l2 match {
        case Cons(x2, xs2) => accumulate(Nil, xs2, Cons(x2, acc))
        case Nil => acc
      }
    }
    reverse(accumulate(l1, l2, Nil:List[Int]))
  }

  /*
    EXERCISE 3.23
  Generalize the function you just wrote so that it’s not specific to integers or addition.
    Name your generalized function zipWith.
*/
  def zipWith[A,B,C](l1: List[A], l2: List[B])(fn: (A,B) => C): List[C] = {
    def accumulate(l1: List[A], l2: List[B], acc: List[C]): List[C] = l1 match{
      case Cons(x1,xs1) => l2 match {
        case Cons(x2, xs2) => accumulate(xs1, xs2, Cons(fn(x1,x2), acc))
        case Nil => throw new Error("Unequal lists: "+ l1 + " / " + l2)
      }
      case Nil => acc
    }
    reverse(accumulate(l1, l2, Nil:List[C]))
  }

  def zipWithIndexFold[A](l: List[A]) : List[(Int,A)] = {
    reverse(
      foldLeft(l, (idx: Int) => Nil:List[(Int,A)]){
        (acc, elem) => {
          (idx: Int) => Cons (
            (acc(idx) match {
              case Nil => 1
              case Cons((idx1,_), _) => idx1 + 1}, elem), acc(idx))
        }
      }(0))
  }

  def zipWithIndex[A](l: List[A]): List[(A,Int)] = {
    def accumulator(l1: List[A], idx: Int, acc: List[(A, Int)]) : List[(A, Int)] = l1 match {
      case Nil => acc
      case Cons(x, xs) => accumulator(xs, idx + 1, Cons((x, idx), acc))
    }
    reverse(accumulator(l,1,Nil))
  }

/*
  Ex 3.24
  As an example, implement hasSubsequence for checking whether a List contains
  another List as a subsequence. For instance, List(1,2,3,4) would have
  List(1,2), List(2,3), and List(4) as subsequences, among others. You may have
  some difficulty finding a concise purely functional implementation that is also efficient.
  That’s okay. Implement the function however comes most naturally. We’ll
  return to this implementation in chapter 5 and hopefully improve on it. Note: Any
  two values x and y can be compared for equality in Scala using the expression x == y.
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean
 */

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    find(sub, enumerateSubSequences(sup))
  }

  def innerAccumulate[A](l1: List[A], acc: List[List[A]]) : List[List[A]] = l1 match {
    case Cons(x, xs) => innerAccumulate(xs, Cons(l1, acc))
    case Nil => acc
  }

  def enumerateSubSequences[A](l:List[A]): List[List[A]] = l match {
    case Cons(x, xs) => innerAccumulate(l, enumerateSubSequences(init(l)))
    case Nil => Nil
  }

  def find[A](item:A, l: List[A]) : Boolean = l match {
    case Cons(x, xs) if x == item => true
    case Cons(x, xs) => find(item, xs)
    case Nil => false
  }



  def head[A](l: List[A]): A = l match {
    case Nil => throw new Exception("Called head on empty list")
    case Cons(x, xs) => x
  }

  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(x, xs) => xs
  }

  def setHead[A](l: List[A], newHead: A): List[A] = l match {
    case Nil => Nil
    case Cons(x, xs) => Cons(newHead, xs)
  }

  def len[A](l: List[A]): Int = {
    @annotation.tailrec
    def lenInternal(l: List[A], acc: Int): Int = l match {
      case Nil => acc
      case Cons(x, xs) => lenInternal(xs, acc + 1)
    }
    lenInternal(l, 0)
  }

  @annotation.tailrec
  def getLast[A](l: List[A]): A = l match {
    case Cons(x, Nil) => x
    case Cons(x, xs) => getLast(xs)
    case Nil => throw new Exception("No last element of empty list")
  }

  //def append[A](l: List[A], elem: A) = reverse(Cons(elem, reverse(l)))

  def init[A](l: List[A]) = reverse(drop(reverse(l), 1))

  @annotation.tailrec
  def drop[A](l: List[A], n: Int): List[A] = {
    if (n == 0) l
    else if (tail(l) == Nil) Nil
    else drop(tail(l), n - 1)
  }

  @annotation.tailrec
  def dropWhile[A](l: List[A])(cond: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(x, xs) if !cond(x) => l
    case Cons(x, xs) => dropWhile(tail(l))(cond)
  }

  /*def reverse[A](l: List[A]): List[A] = {
    @annotation.tailrec
    def reverseInternal[A](l: List[A], out: List[A]): List[A] = l match {
      case Nil => out
      case Cons(x, xs) => reverseInternal(xs, Cons(x, out))
    }
    reverseInternal(l, Nil)
  }*/

  def apply[A](as: A*): List[A] = as
}

object ListTest {
  def main(args: Array[String]): Unit = {
    val a1 = Cons(1, Cons(2, Nil))
    println(List.sum(a1))

    val d1 = Cons(1.0, Cons(4.0, Cons(5.0, Cons(6.0, Nil))))
    println(List.product(d1))

    println(List.sum(List(1, 2, 3, 4)))

    val x = List(1, 2, 3, 4, 5) match {
      case Cons(r, Cons(2, Cons(4, _))) => r
      case Nil => 42
      case Cons(d, Cons(v, Cons(3, Cons(4, _)))) => d + v
      case Cons(h, t) => h + List.sum(t)
      case _ => 101
    }
    println(x)
    val l1 = List(1, 2, 3, 4)
    println(l1)
    println(List.tail(l1))
    println(List.setHead(l1, 9))
    println(List.drop(l1, 3))
    println(List.dropWhile(l1)(_ <= 3))
    println(List.reverse(l1))
    println(List.append(l1, 5))
    println(List.getLast(l1))
    println(List.foldSum(l1))
    println(List.init(l1))
    println(List.foldProduct(List(1.0,2.0,3.0,4.0)))
    println(List.foldProduct(List(2.0,0.0,3.0,0.0)))


    /*
  EXERCISE 3.8
  See what happens when you pass Nil and Cons themselves to foldRight, like this:
  foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_)).10 What do you think this
  says about the relationship between foldRight and the data constructors of List?
 */
    println(List.foldRight(List(1,2,3), Nil:List[Int])(Cons(_, _))) // => List(1,2,3)
    // This indicates a few things -
    // The data constructor is the same signature as the expected function in foldRight
    // The foldright function builds in the same order as the initial list

    println(List.length(l1))
    println(List.foldLeftSum(l1))
    println(List.foldLeftSum(l1) == List.foldSum(l1))

    println(List.reverse(l1))

    println(List.foldRight(l1, 0)(_ + _))

    println(List.foldRight(List(1,2,3), Nil:List[Int])(Cons(_, _)))
    println(List.newFoldLeft(List(1,2,3), Nil:List[Int])((a,b) => Cons(b,a)) == List.foldLeft(List(1,2,3), Nil:List[Int])((a,b) => Cons(b,a)))
    println(List.concat(List(List(1,2,3), List(4,5,6))))
    println(List.add1(List(1,2,3)))
    println(List.doubleToString(List(1.2,3.4,5.2434,5.4444)))
    println(List.map(List(1,2,3))(_ + 1))
    println(List.filter(List(1,2,3))(_ % 2 == 0))
    println(List.flatMap(List(1,2,3))(i => List(i, i)))
    println(List.filter1(List(1,2,3))(_ % 2 == 1))
    println(List.add(List(1), List(4,5)))
    println(List.zipWith(List(1,2,3),List(4,5,6))(_ + _))
    println(List.zipWith(List("a","b","c"),List(1,2,3))((a,b) => s"$a:$b"))
    println(List.zipWithIndex(List("a","b","c")))
    println(List.hasSubsequence(List("a","b","c"), List("b","c")))
    println(List.hasSubsequence(List("a","b","c"), List("b","d")))
    println(List.hasSubsequence(List("a","b","c"), List("b")))
    println(List.hasSubsequence(List("a","b","c"), Nil))
    println(List.hasSubsequence(List("a","b","c"), List("a","c")))
    println(List.hasSubsequence(Nil, Nil))
    println(List.hasSubsequence(List.append(List("a","b","c"), List("a","d")), List("c","a")))

  }
}

sealed trait Tree[+A]
case class Leaf[A](value:A) extends Tree[A]
case class Branch[A](left: Tree[A], right:Tree[A]) extends Tree[A]

object Tree {
 /*

EXERCISE 3.25
Write a function size that counts the number of nodes (leaves and branches) in a tree.
*/

  def size[A](tree:Tree[A]):Int = {
    def accumulator(tree:Tree[A], acc:Int):Int = tree match {
      case Leaf(a) => acc
      case Branch(l, r) => accumulator(l, acc + 1) + accumulator(r, acc + 1)
    }
    accumulator(tree, 0)
  }

  def size2[A](tree:Tree[A]):Int = tree match {
    case Leaf(a) => 1
    case Branch(l, r) => size2(l) + size2(r)
  }
  /*
EXERCISE 3.26
Write a function maximum that returns the maximum element in a Tree[Int]. (Note:
In Scala, you can use x.max(y) or x max y to compute the maximum of two integers x
and y.)
*/

  def maximum(tree:Tree[Int]): Int = tree match {
    case Leaf(a) => a
    case Branch(l, r) => maximum(l) max maximum(r)
  }

  /*
EXERCISE 3.27
Write a function depth that returns the maximum path length from the root of a tree
to any leaf.
*/
  def depth[A](tree:Tree[A]): Int = tree match {
    case Leaf(a) => 1
    case Branch(l, r) => (depth(l) + 1) max (depth(r) + 1)
  }


  /*
EXERCISE 3.28
Write a function map, analogous to the method of the same name on List, that modifies
each element in a tree with a given function.
*/

  def map[A,B](tree: Tree[A])(fn: A => B) : Tree[B] = tree match {
    case Leaf(a) => Leaf(fn(a))
    case Branch(l, r) => Branch(map(l)(fn), map(r)(fn))
  }

  /*
EXERCISE 3.29
Generalize size, maximum, depth, and map, writing a new function fold that abstracts
over their similarities. Reimplement them in terms of this more general function. Can
you draw an analogy between this fold function and the left and right folds for List?

 */
  def fold[A,B](t: Tree[A])(l: A => B)(b: (B,B) => B): B = t match {
    case Leaf(a) => l(a)
    case Branch(left, right) => b(fold(left)(l)(b), fold(right)(l)(b))
  }

  def foldSize[A](t: Tree[A]) = fold(t)(a => 1)(_ + _)
  def foldMaximum(t: Tree[Int]) = fold(t)(a => a)(_ max _)
  def foldDepth[A](t: Tree[A]) = fold(t)(a => 1)(_ + 1 max _ + 1)
  def foldMap[A, B](t: Tree[A])(fn: A => B) : Tree[B] = fold(t)(a => buildTree(fn(a)))(buildTree)
  def buildTree[A](elem:A): Tree[A] = Leaf(elem)
  def buildTree[A](elem1: Tree[A], elem2: Tree[A]): Tree[A] = Branch(elem1, elem2)

  def equal[A](t1: Tree[A], t2: Tree[A]): Boolean = t1 match {
    case Leaf(v1) => t2 match {
      case Leaf(v2) => v1 == v2
      case _ => false
    }
    case Branch(l1, r1) => t2 match {
      case Leaf(_) => false
      case Branch(l2, r2) => equal(l1, l2) && equal(r1, r2)
    }
  }

}

object treeTest {
  import Tree._
  def main(args:Array[String]): Unit = {
    val tree = Branch(Leaf("A"), Branch(Leaf("B"), Leaf("C")))
    val tree1 = Branch(Leaf("A"), Branch(Leaf("D"), Leaf("C")))
    val tree2 = Branch(Leaf("A"), Leaf("D"))
    val intTree = Branch(Branch(Leaf(5), Leaf(10)), Branch(Branch(Leaf(4), Leaf(20)), Leaf(3)))
    println(tree)
    println(size(tree))
    println(size(tree))
    println(maximum(intTree))
    println(depth(tree))
    println(depth(intTree))
    println(map(intTree)(_ + 1))
    println(foldSize(tree))
    println(foldMaximum(intTree))
    println(foldDepth(intTree))
    println(foldMap(intTree)(_ + 1))
    println(equal(tree,tree))
    println(equal(tree1,tree))
    println(equal(tree, tree2))
  }
}
