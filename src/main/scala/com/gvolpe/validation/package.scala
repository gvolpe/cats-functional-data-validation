package com.gvolpe

package object validation {

  case class Address(houseNumber: Int, streetName: String)
  case class Person(name: String, address: Address)

}
