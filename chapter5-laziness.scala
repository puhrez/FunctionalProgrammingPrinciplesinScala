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
}
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
      case Cons(h, t) if n > 0 => Stream.cons(h(), t() take(n - 1))
      case _ => Stream.empty
  }
  //exercise 5.2
  @annotation.tailrec
  final def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t() drop(n - 1)
    case _ => this
  }
  //exercise 5.3
  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if (p(h())) => Stream.cons(h(), t() takeWhile p)
    case _ => Stream.empty
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
    foldRight(Stream.empty[A])((h, t) => if (p(h)) Stream.cons(h, t) else Stream.empty)
  //exercise 5.6a
  def headOptionViaFoldRight: Option[A] = foldRight(None: Option[A])((h, _) => Some(h))
  //exercise 5.7
  def map[B](f: A => B): Stream[B] = foldRight(Stream.empty[B])((h, t) => Stream.cons(f(h), t))
  def filter(p: A => Boolean): Stream[A] = foldRight(Stream.empty[A])((h, t) => if (p(h)) Stream.cons(h, t) else t)
  def append[B >: A](el: => Stream[B]): Stream[B] = foldRight(el)((h, t) => Stream.cons(h, t))
  def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight(Stream.empty[B])((h, t) => f(h) append t)

  def find(p: A => Boolean): Option[A] = filter(p).headOption
}

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
  f(z) map { case (a, s) => Stream.cons(a, unfold(s)(f))} getOrElse Stream.empty[A]

//exercise 5.12
def constantViaUnfold[A](a: A): Stream[A] = unfold(a)((a) => Some((a,a)))
def fromViaUnfold(n: Int): Stream[Int] = unfold(n)((a) => Some((a + 1, a + 1)))
def onesViaUnfold: Stream[Int] = unfold(1)((_) => Some((1,1)))
def fibsViaUnfold: Stream[Int] = unfold((0, 1))((tup) => Some(tup._1, (tup._2, tup._1 + tup._2)))

//exercise 5.13
