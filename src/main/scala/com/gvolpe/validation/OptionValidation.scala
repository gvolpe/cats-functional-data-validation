package com.gvolpe.validation

object OptionValidation {

  def makeAddress(number: Int, name: String): Option[Address] = {
    if (number <= 0 || name.isEmpty) None
    else Some(Address(number, name))
  }

  def makePerson(name: String, address: Option[Address]): Option[Person] = {
    if (name.isEmpty) None
    else address.flatMap(a => Some(Person(name, a)))
  }

}
