package com.apex.cli.core

object NewAPP {
  def main(args: Array[String]): Unit = {

    println("Welcome to CLI, type \"help\" for command list:")
    while (true) {
      NewCommand.execute(Console.in.readLine()) match {
        case NewUnKnown(cmd) =>
          println(s"unknown command: $cmd")
        case NewInvalidParams(input) =>
          println(s"invalid parameters: $input")
        case NewSuccess(result) =>
          println(result)
        case NewHelp(message) =>
          println(message)
        case NewError(e) =>
          println(e)
        case NewNoInput() => {}
        case NewQuit() =>
          sys.exit()
      }
    }
  }
}
