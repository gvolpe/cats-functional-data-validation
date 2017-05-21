package com.gvolpe.validation

import scala.collection.mutable.MutableList

// sbt 'run-main com.gvolpe.validation.OOPValidation 0 "" "" '
object OOPValidation {

  private def validatePerson(personName: String, notification: Notification) = {
    if (personName.isEmpty)
      notification.addError("Name must not be empty!")
  }

  private def validateAddress(houseNumber: Int, streetName: String, notification: Notification) = {
    if (houseNumber <= 0)
      notification.addError("Street number must be greater than zero!")

    if (streetName.isEmpty)
      notification.addError("Street name must not be empty!")

    if (streetName.length > 10)
      notification.addError("Street name must not have more than 10 characters!")
  }

  def main(args: Array[String]): Unit = {
    require(args.length == 3, "Missing parameters! HouseNumber, StreetName and PersonName.")

    val houseNumber = args.head.toInt
    val streetName  = args.tail.head
    val personName  = args.last

    val notification = new Notification()

    validateAddress(houseNumber, streetName, notification)
    validatePerson(personName, notification)

    val address = Address(houseNumber, streetName)
    val person  = Person(personName, address)

    if (notification.hasErrors) {
      print("ERROR: ")
      println(notification.reportErrors)
    } else {
      println(person)
    }
  }

}

// OOP Notification pattern
// https://martinfowler.com/articles/replaceThrowWithNotification.html
class Notification {
  private val errors = MutableList.empty[String]

  def addError(error: String): Unit = errors.+=(error)

  def hasErrors: Boolean = errors.nonEmpty

  def reportErrors: List[String] = errors.toList
}