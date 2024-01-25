package chp4

enum Option[+A] {
  case Some(get: A)
  case None

  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(a) => Some(f(a))
  }
  def flatMap[B](f: A => Option[B]): Option[B] = {
    this.map(f).getOrElse(None)
  }
  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(a) => a
  }
  def orElse[B >: A](ob: => Option[B]): Option[B] = ob
  def filter(f: A => Boolean): Option[A] = this.flatMap(
    a => if f(a) then Some(a) else None
  )
}

object Option {
  def lift[A, B](f: A => B): Option[A] => Option[B] = _.map(f)

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    a.flatMap(x => b.map(y => f(x, y)))
  }

  def sequence[A](as: List[Option[A]]): Option[List[A]] = {
    as.foldRight[Option[List[A]]](Some(Nil)) {
      (curr, acc) => acc.flatMap(xs => curr.map(a => a :: xs) )
    }
  }

  def traverse[A, B](as:List[A])(f: A => Option[B]): Option[List[B]] = as match {
    case Nil => {
      val emptyList: List[B] = Nil
      Some(emptyList)
    }
    case x :: xs => f(x).flatMap(b => traverse(xs).map(bs => b :: bs))
  }
}
