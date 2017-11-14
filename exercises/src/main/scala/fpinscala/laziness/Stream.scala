package fpinscala.laziness

import Stream._
trait Stream[+A] {

  def toList: List[A] =
    this match {
      case Empty => Nil
      case Cons(h, t) => h() :: t().toList
    }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  final def take(n: Int): Stream[A] = {
    this match {
      case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
      case Cons(h, _) if n == 1 => cons(h(), empty)
      case _ => empty
    }
  }

  def drop(n: Int): Stream[A] =
    this match {
      case Cons(h, t) if n > 0 => t().drop(n - 1)
      case _ => this
    }

  def takeWhile(p: A => Boolean): Stream[A] =
    foldRight(empty: Stream[A])((a, b) => if (p(a)) cons(a, b) else empty)

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)(p(_) && _)

  def headOption: Option[A] =
    foldRight(None: Option[A])((a, _) => Some(a))

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.
  def map[B](f: A => B): Stream[B] =
    foldRight(empty: Stream[B])((h, t) => cons(f(h), t))

  def filter(f: A => Boolean): Stream[A] =
    foldRight(empty[A])((h, t) =>
      if (f(h)) cons(h, t)
      else t)

  def append[B >: A](bs: => Stream[B]): Stream[B] =
    foldRight(bs)(cons(_, _))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty: Stream[B])(f(_) append _)

  def startsWith[A](s: Stream[A]): Boolean =
    zipAll(s).takeWhile(!_._2.isEmpty).forAll { case (a, b) => a == b }

  def mapViaUnfold[B](f: A => B): Stream[B] =
    unfold(this) {
      case Empty => None
      case Cons(h, t) => Some(f(h()), t())
    }

  def takeViaUnfold(n: Int): Stream[A] =
    unfold((this, n)) {
      case (Cons(h, t), 1) => Some(h(), (empty, 0))
      case (Cons(h, t), n) if n > 1 => Some(h(), (t(), n - 1))
      case _ => None
    }

  def takeWhileViaUnfold(p: A => Boolean): Stream[A] =
    unfold(this) {
      case Cons(a, t) if (p(a())) => Some((a(), t()))
      case _ => None
    }

  def zipWith[B, C](s2: Stream[B])(f: (A, B) => C): Stream[C] =
    unfold((this, s2)) {
      case (Cons(h1, t1), Cons(h2, t2)) =>
        Some((f(h1(), h2()), (t1(), t2())))
      case _ =>
        None
    }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] =
    unfold((this, s2)) {
      case (Cons(h1, t1), Empty) => Some(((Some(h1()), None), (t1(), empty)))
      case (Empty, Cons(h2, t2)) => Some((None, Some(h2())), (empty, t2()))
      case (Cons(h1, t1), Cons(h2, t2)) => Some((Some(h1()), Some(h2())), (t1(), t2()))
      case _ => None
    }

  def tails: Stream[Stream[A]] =
    unfold(this) { case Cons(_, t) => Some(t(), t()) case _ => None }

  def scanRight[B](z: B)(f: (A, B) => B): Stream[B] =
    foldRight(Stream(z))((x,acc) => cons(f(x,(acc.headOption) getOrElse z), acc))
}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty 
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  def constant[A](a: A): Stream[A] =
    cons(a, constant(a))

  def from(n: Int): Stream[Int] = 
    cons(n, from(n + 1))

  def fibs: Stream[Int] = {
    def go(x: Int, y: Int): Stream[Int] = 
      cons(x, go(y, x + y))
    go(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = 
    f(z) match {
      case None => empty
      case Some((a,z)) => cons(a, unfold(z)(f)) 
    }

  def fibsWithUnfold: Stream[Int] =
    unfold((0,1)){ case (a,b) => Some((a, (b, a+b))) }

  def fromWithUnfold(n: Int): Stream[Int] = 
    unfold(n)(x => Some(x, x+1))

  def constantWithUnfold(n: Int): Stream[Int] =
    unfold(n)(x => Some(x, x))

  def onesWithUnfold: Stream[Int] =
    unfold(1)(Some(_, 1))
}