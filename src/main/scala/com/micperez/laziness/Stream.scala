package com.micperez.laziness
import Stream._
//companion object is explicitly referenced because REPL.
trait Stream[+A] {
   def headOption: Option[A] = this match {
      case Empty => None
      case Cons(h, t) => Some(h())
  }
  //exercise 5.1
  def toListRecursive: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toListRecursive
  }
  def toList:List[A] = {
    val buf = new collection.mutable.ListBuffer[A]
    @annotation.tailrec
    def go(s: Stream[A]): List[A] = s match {
      case Cons(h, t) => buf += h(); go(t())
      case _ => buf.toList
    }
    go(this)
  }
  //exercise 5.2
  def take(n: Int): Stream[A] = this match {
      case Cons(h, t) if n > 0 => cons(h(), t() take(n - 1))
      case _ => empty
  }
  //exercise 5.2
  @annotation.tailrec
  final def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t() drop(n - 1)
    case _ => this
  }
  //exercise 5.3
  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if (p(h())) => cons(h(), t() takeWhile p)
    case _ => empty
  }
  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }
  def exists(p: A => Boolean): Boolean = foldRight(false)((a, b) => p(a) || b)
  //exercise 5.4
  def forAll(p: A => Boolean): Boolean = foldRight(true)((a, b) => p(a) && b)
  //exercise 5.5
  def takeWhileViaFoldRight(p: A => Boolean) =
    foldRight(empty[A])((h, t) => if (p(h)) cons(h, t) else empty)
  //exercise 5.6a
  def headOptionViaFoldRight: Option[A] = foldRight(None: Option[A])((h, _) => Some(h))
  //exercise 5.7
  def map[B](f: A => B): Stream[B] = foldRight(empty[B])((h, t) => cons(f(h), t))
  def filter(p: A => Boolean): Stream[A] = foldRight(empty[A])((h, t) => if (p(h)) cons(h, t) else t)
  def append[B >: A](el: => Stream[B]): Stream[B] = foldRight(el)((h, t) => cons(h, t))
  def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight(empty[B])((h, t) => f(h) append t)

  def find(p: A => Boolean): Option[A] = filter(p).headOption
  //exercise 5.13
  def mapViaUnfold[B](f: A => B): Stream[B] =
    unfold(this) {
      case Cons(h, t) => Some((f(h()), t()))
      case _ => None
    }
  def takeViaUnfold(n: Int): Stream[A] =
    unfold((this, n)) {
      case (Cons(h, t), n) if n == 1 => Some((h(), (empty, n -1)))
      case (Cons(h, t), n) if n > 0 => Some(h(), (t(), n -1))
      case _ => None
    }
  def takeWhileViaUnfold(p: A => Boolean): Stream[A] =
    unfold(this) {
      case Cons(h, t) if p(h()) => Some(h(), t())
      case _ => None
    }
  def zipWith[B, C](that: Stream[B])(f: (A, B) => C): Stream[C] =
    unfold((this, that)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some((f(h1(), h2()), (t1(), t2())))
      case _ => None
    }
  def zip[B](that: Stream[B]): Stream[(A, B)] =
    zipWith(that)((_, _))
  def zipWithAll[B, C](that: Stream[B])(f: (Option[A], Option[B]) => C): Stream[C] =
    unfold((this, that)) {
    case (Empty, Empty) => None
    case (Empty, Cons(h2, t2)) => Some(f(Option.empty[A], Some(h2())) -> (empty[A], t2()))
    case (Cons(h1, t1), Empty) => Some(f(Some(h1()), Option.empty[B]) -> (t1(), empty[B]))
    case (Cons(h1, t1), Cons(h2, t2)) => Some(f(Some(h1()), Some(h2())) -> (t1(), t2()))
  }
  def zipAll[B](that: Stream[B]): Stream[(Option[A], Option[B])] =
    zipWithAll(that)((_, _))
 def startsWith[A](that: Stream[A]): Boolean =
   zipAll(that).takeWhile(!_._2.isEmpty) forAll {
     case (h, h2) => h == h2
   }
  def tail: Stream[A] = drop(1)
  def tails: Stream[Stream[A]] =
    unfold(this) {
    case Empty => None
    case s => Some((s, s tail))
  } append (Stream(empty))
  def hasSubsequence[A](s: Stream[A]): Boolean =
    tails exists (_ startsWith s)
  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] =
    foldRight((z, Stream(z)))((a, p0) => {
      lazy val p1 = p0
      val b2 = f(a, p1._1)
      (b2, cons(b2, p1._2))
    })._2
}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]
object Stream {
  def cons[A](head: => A, tail: => Stream[A]): Stream[A] = {
    lazy val h = head
    lazy val t = tail
    Cons(() => h, () => t)
  }
  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
    //exercise 5.8
    def constant[A](a: A): Stream[A] = Stream.cons(a, constant(a))

    //exercise 5.9
    def from(n: Int): Stream[Int] =  Stream.cons(n, from(n + 1))

    //exercise 5.10
    def fibs: Stream[Int] = {
      def go(prev: Int, curr: Int): Stream[Int] =
        Stream.cons(prev, go(curr, prev + curr))
      go(0, 1)
    }

    //exercise 5.11
    def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = 
      f(z) map { case (a, s) => cons(a, unfold(s)(f))} getOrElse empty[A]

    //exercise 5.12
    def constantViaUnfold[A](a: A): Stream[A] = unfold(a)((a) => Some((a,a)))
    def fromViaUnfold(n: Int): Stream[Int] = unfold(n)((a) => Some((a + 1, a + 1)))
    def onesViaUnfold: Stream[Int] = unfold(1)((_) => Some((1,1)))
    def fibsViaUnfold: Stream[Int] = unfold((0, 1))((tup) => Some(tup._1, (tup._2, tup._1 + tup._2)))

}
