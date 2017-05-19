package com.gvolpe.validation

import cats.{Applicative, Functor, Semigroup}
import cats.instances.string._
import cats.syntax.cartesian._

object Validation {

  sealed trait Validation[+Err, +A]
  final case class Success[A](value: A) extends Validation[Nothing, A]
  final case class Failure[T](err: T)   extends Validation[T, Nothing]

  type Error      = String
  type Result[A]  = Validation[Error, A]

//  implicit def validationSemigroup[A : Semigroup]: Semigroup[Validation[Error, A]] = new Semigroup[Validation[Error, A]] {
//    def combine(v1: Validation[Error, A], v2: Validation[Error, A]): Validation[Error, A] = (v1, v2) match {
//      case (Success(_), s@Success(_)) => s
//      case (Failure(e1), Failure(e2)) => Failure(Semigroup[Error].combine(e1, e2))
//      case (_, f@Failure(_))          => f
//      case (f@Failure(_), _)          => f
//    }
//  }

  implicit def validationFunctor[T]: Functor[Validation[T, ?]] = new Functor[Validation[T, ?]] {
    override def map[A, B](fa: Validation[T, A])(f: A => B): Validation[T, B] = fa match {
      case f@Failure(_) => f
      case Success(a)   => Success(f(a))
    }
  }

  implicit def validationApplicative[T : Semigroup]: Applicative[Validation[T, ?]] = new Applicative[Validation[T, ?]] {
    override def pure[A](x: A): Validation[T, A] = Success(x)

    override def ap[A, B](ff: Validation[T, A => B])(fa: Validation[T, A]): Validation[T, B] = (ff, fa) match {
      case (Success(_), f@Failure(_)) => f
      case (f@Failure(_), Success(_)) => f
      case (Failure(e1), Failure(e2)) => Failure(Semigroup[T].combine(e1, e2))
      case (Success(f), Success(x))   => Success(f(x))
    }
  }

  private def streetNumberGreaterThanZero(number: Int): Result[Int] = {
    if (number <= 0) Failure("Number must be greater than zero!")
    else Success(number)
  }

  private def notEmpty(name: String, error: Error): Result[String] = {
    if (name.isEmpty) Failure(error)
    else Success(name)
  }

  private def streetNameNotEmpty(name: String): Result[String] =
    notEmpty(name, "Street name must not be empty!")

  def makeAddress(number: Int, name: String): Result[Address] = {
//    val pf = (Address.apply _).curried
//    val ff = Functor[Validation[Error, ?]].map(streetNumberGreaterThanZero(number))(pf)
//    Applicative[Validation[Error, ?]].ap(ff)(streetNameNotEmpty(name))
    (streetNumberGreaterThanZero(number) |@| streetNameNotEmpty(name)).map(Address.apply)
  }

}