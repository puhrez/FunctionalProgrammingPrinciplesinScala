case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]
//excercise 4.1
sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(x) => Some(f(x))
  }
  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(x) => x
  }
  def orElse[B >: A](ob: => Option[B]): Option[B] = Some(this) getOrElse ob
  def flatMap[B](f: A => Option[B]): Option[B] = map(f) getOrElse None
  def filter(f: A => Boolean): Option[A] = flatMap(a => if (f(a)) Some(a) else None)
}

def mean(xs: Seq[Double]): Option[Double] =
  if (xs.isEmpty) None
  else Some(xs.sum / xs.length)

//exercise 4.2
def variance(xs: Seq[Double]): Option[Double] =
  mean(xs) flatMap (m => mean(xs.map(a => math.pow(a - m, 2))))

def lift[A,B](f: A => B): Option[A] => Option[B] = _ map f

def Try[A](a: => A): Option[A] =
  try Some(a)
  catch {case e: Exception => None}


//exercise 4.3
def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
  a flatMap(aa => b map (bb => f(aa, bb)))

//exercise 4.4
def sequence[A](a: List[Option[A]]): Option[List[A]] =
  a.foldRight[Option[List[A]]](Some(Nil))((h, t) => map2(h,t)(_ :: _))

//exercise 4.5
def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
  a.foldRight[Option[List[B]]](Some(Nil))((h, t) => map2(f(h), t)(_ :: _))

def sequenceViaTraverse[A](a: List[Option[A]]): Option[List[A]] = traverse(a)(a => a)

//Either
case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

//exercise 4.6
sealed trait Either [+E, +A] {
  def map[B](f: A => B): Either[E, B] = this match {
    case Left(x) => Left(x)
    case Right(x) => Right(f(x))
  }
  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Right(x) => f(x)
    case Left(x) => Left(x)
  }
  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Right(x) => Right(x)
    case Left(_) => b
  }
  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    for {
      a <- this
      bb <- b
    } yield f(a, bb)
}

def mean(xs: IndexedSeq[Double]): Either[String, Double] =
  if (xs.isEmpty)
    Left("mean of empty List")
  else
    Right(xs.sum / xs.length)

def safeDiv(x: Int, y: Int): Either[Exception, Int] =
  try Right(x/y)
  catch { case e: Exception => Left(e) }

def Try[A](a: => A): Either[Exception, A] =
  try Right(a)
  catch { case e: Exception => Left(e) }

//exercise 4.7
def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
  as.foldRight[Either[E, List[B]]] (Right(Nil))((x, y) => f(x).map2 (y)(_ :: _))
def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = traverse(es)(x => x)



//fuck man...




