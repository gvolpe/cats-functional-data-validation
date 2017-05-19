package com.gvolpe.validation

import cats.Apply
import cats.data.{Validated, ValidatedNel}
import cats.data.Validated._

object CatsValidation {

  type Error        = String
  type Result[A]    = Validated[Error, A]
  type ResultNel[A] = ValidatedNel[Error, A]

  private def streetNumberGreaterThanZero(number: Int): Result[Int] = {
    if (number <= 0) Invalid("Number must be greater than zero!")
    else Valid(number)
  }

  private def notEmpty(name: String, error: Error): Result[String] = {
    if (name.isEmpty) Invalid(error)
    else Valid(name)
  }

  private def streetNameNotEmpty(name: String): Result[String] =
    notEmpty(name, "Street name must not be empty!")

  def makeAddress(number: Int, name: String): ResultNel[Address] =
    Apply[ValidatedNel[Error, ?]].map2 (
      streetNumberGreaterThanZero(number).toValidatedNel,
      streetNameNotEmpty(name).toValidatedNel
    ) {
      case (validNumber, validName) => Address(validNumber, validName)
    }

  private def nameNotEmpty(name: String): Result[String] =
    notEmpty(name, "Name must not be empty!")

  def makePerson(name: String, address: ResultNel[Address]): ResultNel[Person] =
    Apply[ValidatedNel[Error, ?]].map2(
      nameNotEmpty(name).toValidatedNel,
      address
    ) {
      case (validName, validAddress) => Person(validName, validAddress)
    }

}
