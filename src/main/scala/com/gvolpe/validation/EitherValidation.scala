package com.gvolpe.validation

import cats.syntax.either._

object EitherValidation {

  type Error = String

  private def streetNumberGreaterThanZero(number: Int): Either[Error, Int] = {
    if (number <= 0) Left("Number must be greater than zero!")
    else Right(number)
  }

  private def notEmpty(name: String, error: Error): Either[Error, String] = {
    if (name.isEmpty) Left(error)
    else Right(name)
  }

  private def streetNameNotEmpty(name: String): Either[Error, String] =
    notEmpty(name, "Street name must not be empty!")

  def makeAddress(number: Int, name: String): Either[Error, Address] =
    for {
      validNumber <- streetNumberGreaterThanZero(number)
      validName   <- streetNameNotEmpty(name)
    } yield Address(validNumber, validName)

  private def nameNotEmpty(name: String): Either[Error, String] =
    notEmpty(name, "Name must not be empty!")

  def makePerson(name: String, address: Either[Error, Address]): Either[Error, Person] =
    for {
      validName     <- nameNotEmpty(name)
      validAddress  <- address
    } yield Person(validName, validAddress)

  def makePerson2(name: String, address: Either[Error, Address]): Either[Error, Person] = {
    nameNotEmpty(name).flatMap { validName =>
      address.map { validAddress =>
        Person(validName, validAddress)
      }
    }
  }

}
