package de.uni_saarland.cs.se
package util

import java.io.{ByteArrayOutputStream, OutputStream}

/**
  * Simple logger class that allows easy access to logged messages for testing.
  */
class Logger {
  private val outputStream: OutputStream = ByteArrayOutputStream()

  def log(message: String): Unit = {
    Console.withOut(outputStream) {
      println(message)
    }
  }

  def lastLoggedMessage(): String = {
    outputStream.toString.split("\\R").last
  }
}
