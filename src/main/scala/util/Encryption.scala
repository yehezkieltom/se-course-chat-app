package de.uni_saarland.cs.se
package util

/**
  * Interface for encryption methods.
  */
trait EncryptionMethod {
  def encrypt(str: String): String
  def decrypt(str: String): String
}

/**
  * ROT13 "encryption".
  */
case object ROT13 extends EncryptionMethod {
  override def encrypt(str: String): String = {
    str.toCharArray.map {
      case c if (c >= 'A') && (c <= 'Z') => (((c - 'A' + 13) % 26) + 'A').toChar
      case c if (c >= 'a') && (c <= 'z') => (((c - 'a' + 13) % 26) + 'a').toChar
      case c                             => c
    }.mkString
  }

  override def decrypt(str: String): String = encrypt(str)
}

/**
  * String-reversal "encryption".
  */
case object REVERSE extends EncryptionMethod {
  override def encrypt(str: String): String = str.reverse

  override def decrypt(str: String): String = encrypt(str)
}
