package com.apex.cli

import scala.io.StdIn


object APP {

  def main(args: Array[String]): Unit = {
    
    System.out.println("hello world")

    var break = false
    while (!break) {
      val command = Console.in.readLine()
      if (command.trim.toLowerCase.equals("quit")) {
        break = true
      } else {
        System.out.println(command)
      }
    }
  }
}
