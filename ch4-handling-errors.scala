import scala.{Option => _, Some => _, None => _}

sealed trait Option[+A] {
  //EX 4.1
  def map[B](f: A => B): Option[B] = this match {
    case Some(v) => Some(f(v))
    case None => None
  }
  def getOrElse[B>:A](default: => B): B = this match {
    case Some(v) => v
    case None => default
  }
  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case Some(v) => f(v)
    case None => None
  }
  assert(Some(1).flatMap(v => Some(v+1)) == Some(2))

  def flatMapM[B](f: A => Option[B]): Option[B] =
    map(f) getOrElse None
  assert(Some(1).flatMapM(v => Some(v+1)) == Some(2))

  def orElse[B>:A](ob: => Option[B]): Option[B] = this match {
    case Some(v) => Some(v)
    case None => ob
  }
  assert(None.orElse(Some(2)) == Some(2))

  def orElseM[B>:A](ob: => Option[B]): Option[B]=
    this map(Some(_)) getOrElse ob
  assert(None.orElseM(Some(2)) == Some(2))

  def filter(f: A => Boolean): Option[A] = flatMap{ v =>
    if (f(v) == true) Some(v) else None
  }
  assert(Some(2).filter(_%2==0) == Some(2))

}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]
object Option{
  //EX 4.2
  def variance(xs: Seq[Double]): Option[Double] = {
    val mean = (xs: Seq[Double]) => if (xs.isEmpty) None else Some(xs.sum/xs.length)
    mean(xs) flatMap (m => mean(xs.map(x => math.pow(x - m, 2))))
  }
  assert(variance(Seq(1,2,3,4,5)) == Some(2))

  def lift[A,B](f: A => B): Option[A] => Option[B] =
    _ map f
  // lift[Int,String](_.toString)

  //EX 4.3
  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A,B) => C): Option[C] =
    a.flatMap(av => b.map(bv => f(av,bv)))
  assert(map2(Some(1),Some(2))(_*_) == Some(2))

  //EX 4.4
  //my original solution
  def sequence[A](a: List[Option[A]]): Option[List[A]] =
    a.foldRight[Option[List[A]]](Some(Nil))((h,t) => map2(h,t)(_ :: _))
  assert(Option.sequence(List(Some(1),Some(2))) == Some(List(1,2)))

  //clever! - works as map2
  def sequenceAlt[A](a: List[Option[A]]): Option[List[A]] =
    a match {
      case Nil => Some(Nil)
      case h :: t => h flatMap (hv => sequenceAlt(t) map (hv :: _))
    }
  assert(Option.sequenceAlt(List(Some(1),Some(2))) == Some(List(1,2)))

  //EX 4.5
  //my original solution (probably inspired by clever hack above ;)
  def traverse[A,B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a match {
      case Nil => Some(Nil)
      case h :: t => map2(f(h),traverse(t)(f))(_ :: _)
    }

  def traverseAlt[A,B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a.foldRight[Option[List[B]]](Some(Nil))((h,t) => map2(f(h),t)(_::_))

  def sequenceT[A](a: List[Option[A]]): Option[List[A]] =
    traverse(a)(identity)
}

import scala.util.{Either => _, Right => _, Left => _}
sealed trait Either[+E, +A]{
  //EX 4.6
  def map[B](f: A => B): Either[E,B] = this match {
    case Left(v) => Left(v)
    case Right(v) => Right(f(v))
  }
  def flatMap[EE >: E, B](f: A => Either[EE,B]): Either[EE,B] =
    this match {
      case Left(v) => Left(v)
      case Right(v) => f(v)
    }
  def orElse[EE >: E, B >: A](b: => Either[EE,B]): Either[EE,B] =
    this match {
      case Right(v) => Right(v)
      case Left(v) => b
    }
  def map2[EE >: E, B, C](b: Either[EE,B])(f: (A,B) => C): Either[EE,C] =
    this.flatMap(v => b.map(bb => f(v,bb)))

}
case class Left[+E](value: E) extends Either[E,Nothing]
case class Right[+A](value: A) extends Either[Nothing,A]

object Either{
  //EX 4.7
  def traverse[E,A,B](as: List[A])(f: A => Either[E,B]): Either[E,List[B]] =
    as match {
      case Nil => Right(Nil)
      case h :: t => (f(h) map2 traverse(t)(f))(_ :: _)
    }
  def sequence[E,A](es: List[Either[E,A]]): Either[E, List[A]] =
    traverse(es)(identity)
}

assert(Right(1).map(_.toString) == Right("1"))
assert(Right(1).flatMap(v => Left(v.toString)) == Left("1"))
assert(Left(1).orElse(Right(2)) == Right(2))
assert(Right(1).map2(Right(2))(_+_) == Right(3))
assert(Either.traverse[Int,Int,Int](List[Int](1,2,3)){v:Int => if(v%2==0) Right(v) else Left(v)} == Left(1))
