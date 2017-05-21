package com.gvolpe.validation

// sbt "run-main com.gvolpe.validation.PrimitiveValidation2 0 Dawson Gabi"
object PrimitiveValidation {

  def makeAddress(number: Int, name: String): Address = {
    if (number <= 0) {
      throw new Exception("Street number must be greater than zero!")
    } else if (name.isEmpty) {
      throw new Exception("Street name must not be empty!")
    } else if (name.length > 10) {
      throw new Exception("Street name must not have more than 10 characters!")
    } else {
      Address(number, name)
    }
  }

  def makePerson(name: String, address: Address): Person = {
    if (name.isEmpty) {
      throw new Exception("Name must not be empty!")
    } else if (address == null) {
      throw new IllegalArgumentException()
    } else {
      Person(name, address)
    }
  }

  def main(args: Array[String]): Unit = {
    require(args.length == 3, "Missing parameters! HouseNumber, StreetName and PersonName.")

    val houseNumber = args.head.toInt
    val streetName  = args.tail.head
    val personName  = args.last

    try {
      val address = makeAddress(houseNumber, streetName)
      val person  = makePerson(personName, address)
      println(person)
    } catch {
      case e: Exception => println(e.getMessage)
    }
  }

}
