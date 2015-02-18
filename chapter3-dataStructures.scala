
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

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

//exercise 3.1
//3

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
def initWithBuffer[A](l: List[A]): List[A] = {
  import collection.mutable.ListBuffer
  var buf = new ListBuffer[A]
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

//exercise 3.14
def append[A](as: List[A], x: List[A]): List[A] = foldRight(as, x)(Cons(_, _))

//exercise 3.15
def flatten[A](as: List[List[A]]): List[A] = foldRight(as, Nil:List[A])(append)

//exercise 3.16
def addOne(ns: List[Int]): List[Int] = foldRight(ns, Nil: List[Int])((h, t) => Cons(h + 1, t))

//exercise 3.17
def eachToString(ds: List[Double]): List[String] = foldRight(ds, Nil: List[String])((h, t) => Cons(h.toString, t))

//exercise 3.18
def map[A, B](as: List[A])(f: A => B): List[B] = foldRight(as, Nil: List[B])((h, t) => Cons(f(h), t))

//exercise 3.19
def filter[A](as: List[A])(f: A => Boolean): List[A] = foldRight(as, Nil: List[A])((h, t) => if (f(h)) Cons(h, t) else t);

//exercise 3.20
def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = flatten(map(as)(f));

//exercise 3.21
def filterViaFlatMap[A](as: List[A])(f: A => Boolean): List[A] = flatMap(as)(x => if (f(x)) List(x) else List());

//exercise 3.22
def pairwiseAdd(l: List[Int], r: List[Int]): List[Int] = (l, r) match {
  case (Nil, _) => Nil
  case (_, Nil) => Nil
  case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, pairwiseAdd(t1, t2))
}

//exercise 3.23
def zipWith[A](l: List[A], r: List[A])(f: (A, A) => A): List[A] = (l, r) match {
  case (Nil, _) => Nil
  case (_, Nil) => Nil
  case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
}

//exercise 3.24
def startsWith[A](sup: List[A], sub: List[A]): Boolean = (sup, sub) match {
  case (_, Nil) => true
  case (Cons(h1, t1), Cons(h2, t2)) if h1 == h2 => startsWith(t1, t2)
  case _ => false
}
def hasSequence[A](sup: List[A], sub: List[A]): Boolean = sup match {
  case Nil => false
  case Cons(h, t) if startsWith(l, sub) => true
  case Cons(_, t) => hasSequence(t, sub)
}

//exercise 3.25
def size[A](t: Tree[A]): Int = t match {
  case Leaf(_) => 1
  case Branch(l, r) => 1 + size(l) + size(r)
}

//exercise 3.26
def maximum(t: Tree[Int]): Int = t match {
  case Leaf(x) => x
  case Branch(l, r) => maximum(l) max maximum(r)
}

//exercise 3.27
def depth[A](t: Tree[A]): Int = t match {
  case Leaf(x) => 0
  case Branch(l, r) => 1 + (depth(l) max depth(r))
}

//exercise 3.28
def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
  case Leaf(x) => Leaf(f(x))
  case Branch(l, r) => Branch(map(l)(f), map(r)(f))
}

//exercise 3.29
def fold[A, B](t: Tree[A])(b: A => B)(f: (B, B) => B): B = t match {
  case Leaf(x) => b(x)
  case Branch(l, r) => f(fold(l)(b)(f), fold(r)(b)(f))
}
def size[A](t: Tree[A]): Int = fold(t)(x => 1)(1 + _ + _)
def maximum(t: Tree[Int]): Int = fold(t)(x => x)(_ max _)
def depth[A](t: Tree[A]): Int = fold(t)(x => 0)((l, r) => 1 + (l max r))
def map[A, B](t: Tree[A])(f: A => B): Tree[B] = fold(t)(x => Leaf(f(x)): Tree[B])((l, r) => Branch(l, r))


