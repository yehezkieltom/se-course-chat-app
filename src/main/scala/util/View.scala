package de.uni_saarland.cs.se
package util

import java.io.{ByteArrayOutputStream, OutputStream}
import scala.Console.RESET

/**
  * Simple view class that simulates a console-based user interface and allows
  * easy acces to printed messages for testing purposes.
  */
class View {
  private val outputStream: OutputStream = ByteArrayOutputStream()

  def printMessage(sender: Int, message: String): Unit = {
    Console.withOut(outputStream) {
      println(s"[$sender] $message")
    }
  }

  def printMessage(sender: Int, message: String, color: String): Unit = {
    Console.withOut(outputStream) {
      println(s"$RESET$color[$sender] $message$RESET")
    }
  }
  
  def lastDisplayedMessage(): String = {
    outputStream.toString.split("\\R").last
  }
}