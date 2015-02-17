package fpscala.datastructures

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
}

def sum(ints: List[Int]): Int = ints match {
  case Nil => 0
  case Cons(x, xs) => x + sum(xs)
}

def product(ds: List[Double]): Double = ds match {
  case Nil => 1.0
  case Cons(x, xs) => x * product(xs)
}

//exercise 3.1
3

//exercise 3.2
def tail[A](l: List[A]): List[A] = l match {
  case Nil => Nil
  case Cons(_, xs) => xs
}

//exercise 3.3
def setHead[A](l: List[A], h: A): List[A] = l match {
  case Nil => Nil
  case Cons(_, xs) => Cons(h, xs)
}

//exercise 3.4
def drop[A](l: List[A], n: Int): List[A] = {
  if (n <= 0) l
  else l match {
    case Nil => Nil
    case Cons(_, xs) => drop(xs, n - 1)
  }
}

//exercise 3.5
def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = l match {
  case Nil => Nil
  case Cons(x, _) if !f(x) => l
  case Cons(_, xs) => dropWhile(xs)(f)
}

//exercise 3.6
def init[A](l: List[A]): List[A] = l match {
  case Nil => Nil
  case Cons(_, Nil) => Nil
  case Cons(x, xs) => Cons(x, init(xs))
}
//it doesn't operate in constant time because it has to iterate through the list
//book has more efficient implementation with buffer
def initWithBuffer[A](l: List[A]): List[A] = l match {
  import collection.mutable.ListBuffer
  @annotation.tailrec
  def go(cur: List[A]): List[A] = cur match {
    case Nil => Nil
    case Cons(_, Nil) => List(buf.toList: _*)
    case Cons(h, t) => buf += h; go(t)
  }
  go(l)
}


def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = as match {
  case Nil => z
  case Cons(x, xs) => f(x, foldRight(xs, z)(f))
}
def sum2(ns: List[Int]) = foldRight(ns, 0)(_ + _)
def product2(ds: List[Double]) = foldRight(ds, 1.0)(_ * _)

//exercise 3.7
/*No because you can't dynamically add cases to a match statement, moreover
 * the burden of adding a boolean function parameter would nullify the point of
 * foldRight which is generalization
 */

//exercise 3.8
/*it just replaces the constructors Nil and Cons with z and f, respectively
 * so using the constructors in those places will yield teh same list
 */

//exercise 3.9
def length[A](as: List[A]): Int = foldRight(as, 0)((_,acc) => acc + 1)

//exercise 3.10
@annotation.tailrec
def foldLeft[A, B](as: List[A], z: B)(f: (B,A) => B): B = as match {
  case Nil => z
  case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
}

//exercise 3.11
def sum3(ns: List[Int]) = foldLeft(ns, 0)(_ + _)
def product3(ds: List[Int]) = foldLeft(ds, 1.0)(_ * _)
def length2[A](as: List[A]) = foldLeft(as, 0)((acc, _) => acc + 1)

//exercise 3.12
def reverse[A](as: List[A]) = foldLeft(as, List[A]())((acc, h) => Cons(h, acc))

//exercise 3.13
def foldRightViaFoldLeft[A, B](as: List[A], z: B)(f: (A, B) => B): B =
  foldLeft(reverse(as), z)((acc, h) => f(h, acc))

def foldLeftViaFoldRight[A,B](as: List[A], z: B)(f: (B, A) => B): B=
  foldRight(as, z)((h, acc) => f(h, acc))

//exercise 3.14
def append[A](as: List[A], x: List[A]): List[A] = foldRight(as, x)(Cons(_, _))

//exercise 3.14
def flatten[A](as: List[List[A]]): List[A] = foldRight(as, Nil:List[A])(append)



