package me.chuwy.otusbats

trait Monad[F[_]] extends Functor[F] { self =>
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

  def point[A](a: A): F[A]

  def flatten[A](fa: F[F[A]]): F[A] = flatMap(fa)(fa => fa)
}

object Monad {

  def apply[F[_]](implicit instance: Monad[F]): Monad[F] = instance

  implicit val monadOption: Monad[Option] = new Monad[Option] {
    override def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa.flatMap(f)

    override def point[A](a: A): Option[A] = Some(a)

    override def map[A, B](fa: Option[A])(f: A => B): Option[B] = fa.map(f)
  }

  implicit val monadList: Monad[List] = new Monad[List] {
    override def flatMap[A, B](fa: List[A])(f: A => List[B]): List[B] = fa.flatMap(f)

    override def point[A](a: A): List[A] = List(a)

    override def map[A, B](fa: List[A])(f: A => B): List[B] = fa.map(f)
  }

  type StringEither[A] = Either[String, A]

  implicit val monadStringEither: Monad[Monad.StringEither] = new Monad[Monad.StringEither] {
    override def flatMap[A, B](fa: StringEither[A])(f: A => StringEither[B]): StringEither[B] = fa.flatMap(f)

    override def point[A](a: A): StringEither[A] = Right(a)

    override def map[A, B](fa: StringEither[A])(f: A => B): StringEither[B] = fa.map(f)
  }

}
