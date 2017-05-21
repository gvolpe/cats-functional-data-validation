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

  implicit def validationSemigroup[T : Semigroup, A]: Semigroup[Validation[T, A]] = new Semigroup[Validation[T, A]] {
    override def combine(v1: Validation[T, A], v2: Validation[T, A]): Validation[T, A] = (v1, v2) match {
      case (Success(_), s@Success(_)) => s
      case (Failure(e1), Failure(e2)) => Failure(Semigroup[T].combine(e1, e2))
      case (_, f@Failure(_))          => f
      case (f@Failure(_), _)          => f
    }
  }

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

  private def streetNameLength(name: String): Result[String] = {
    if (name.length > 10) Failure("Street name must not exceed 10 characters!")
    else Success(name)
  }

  private def streetNameValidation(name: String): Result[String] =
    Semigroup[Validation[Error, String]].combine(
      streetNameNotEmpty(name),
      streetNameLength(name)
    )

  def makeAddress(number: Int, name: String): Result[Address] = {
    (streetNumberGreaterThanZero(number) |@| streetNameValidation(name)).map(Address.apply)
  }

  private def personNameNotEmpty(name: String): Result[String] =
    notEmpty(name, "Person name must not be empty!")

  def makePerson(name: String, address: Result[Address]): Result[Person] = {
    (personNameNotEmpty(name) |@| address).map(Person.apply)
  }

  def makeAddressAlt(number: Int, name: String): Result[Address] = {
    val pf = (Address.apply _).curried
    val ff = Functor[Validation[Error, ?]].map(streetNumberGreaterThanZero(number))(pf)
    Applicative[Validation[Error, ?]].ap(ff)(streetNameNotEmpty(name))
  }

}