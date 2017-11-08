package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  def tail[A](l: List[A]): List[A] = 
    l match {
      case Nil        => Nil
      case Cons(_,xs) => xs
    }

  def setHead[A](l: List[A], h: A): List[A] = 
    l match {
      case Nil         => Nil
      case Cons(x,xs)  => Cons(h,xs)
    }

  def drop[A](l: List[A], n: Int): List[A] = 
    if(n == 0) l
    else l match {
           case Nil         => Nil
           case Cons(x, xs) => drop(xs, n-1)
         }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
    l match {
      case Nil        => Nil
      case Cons(x,xs) => if (f(x)) dropWhile(xs, f)
                         else l
    }

  def init[A](l: List[A]): List[A] = 
    l match {
      case Nil         => Nil
      case Cons(_,Nil) => Nil
      case Cons(x,xs)  => Cons(x,init(xs))
    }

  def length[A](l: List[A]): Int = 
    foldRight(l, 0)((_,y) => y+1)


  @annotation.tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = {
    l match {
      case Nil         => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }
  }

  def sumLeft(ints: List[Int]): Int = 
    foldLeft(ints, 0)(_+_)

  def productLeft(ints: List[Int]): Int =
    foldLeft(ints, 1)(_*_)

  def lengthLeft[A](as: List[A]): Int = 
   foldLeft(as, 0)((b,_) => b+1)

  def reverse[A](as: List[A]): List[A] =
    foldLeft(as, List[A]())((bs, a) => Cons(a, bs))

  def foldLeft3[A,B](l: List[A], z: B)(f: (B,A) => B): B =
    foldRight(l, (b:B) => b)((a,g) => b => g(f(b,a)))(z)

  def append2[A](a1: List[A], a2: List[A]): List[A] =
    foldRight(a1, a2)(Cons(_,_))

  def concat[A](ass: List[List[A]]): List[A] =
    foldRight(ass, List[A]())(append(_,_))

  def add1(l: List[Int]): List[Int] =
    foldRight(l, Nil:List[Int])((a,b) => Cons(a+1,b))

  def toStrings(l:List[Double]) =
    foldRight(l, Nil:List[String])((a,b) => Cons(a.toString(), b))

  def map[A,B](l: List[A])(f: A => B): List[B] = 
    foldRight(l, Nil:List[B])((a,b) => Cons(f(a), b))
  
  def filter[A](l: List[A])(f: A => Boolean): List[A] =
    foldRight(l, Nil:List[A])((x,y) => if(f(x)) Cons(x,y) else y)

  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] =
    foldLeft(as, Nil:List[B])((bs, a) => append(bs, f(a)))

  def filter2[A](l: List[A])(f: A => Boolean): List[A] =
    flatMap(l)(a => if(f(a)) List(a) else List[A]())

  def head[A](l: List[A]): A = 
    l match {
      case Nil         => throw new Exception()
      case Cons(x, xs) => x
    }

  def addPairwise(as: List[Int], bs: List[Int]): List[Int] =
    as match {
      case Nil        => Nil
      case Cons(x,xs) => Cons(x + head(bs), addPairwise(xs, tail(bs)))
    }

  def zipWith[A](a1: List[A], a2: List[A])(f: (A, A) => A): List[A] =
    (a1, a2) match {
      case (Nil, _)                     => Nil
      case (_, Nil)                     => Nil
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1,h2), zipWith(t1, t2)(f))
    }

  def startsWith[A](sup: List[A], sub: List[A]): Boolean =
    (sup, sub) match {
      case (Nil, _)       => false
      case (_, Nil)       => true
      case (Cons(a,as), Cons(b,bs)) => a == b && startsWith(as, bs)
    }

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean =
    sup match {
      case Nil    => false
      case Cons(x,xs) => startsWith(sup, sub) || hasSubsequence(xs, sub)
    }
}
