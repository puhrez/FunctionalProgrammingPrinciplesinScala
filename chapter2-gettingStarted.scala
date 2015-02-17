//exercise 2.1
def fib(n: Int): Int = {
  @annotation.tailrec
  def go(n: Int, first: Int, second: Int): Int = n match {
    case 0 => first
    case _ => go(n - 1, second, first + second)
  }
  go(n, 0, 1);
}

//exercise 2.2
def isSorted[A](as: Array[A])(ordered: (A, A) => Boolean): Boolean = {
  def loop(n: Int): Boolean = {
    if (n >= as.length) true
    else if (!ordered(as(n - 1), as(n))) false
    else loop(n+1)
  }
  loop(1)
}

//exercise 2.3
def curry[A,B,C](f: (A,B) => C): A => (B => C) = a => b => f(a,b)

//exercise 2.4
def uncurry[A,B,C](f: A => B => C): (A,B) => C = (a,b) => f(a)(b)

//exercise 2.5
def compose[A,B,C](f: B => C, g: A => B): A => C = x => f(g(x))