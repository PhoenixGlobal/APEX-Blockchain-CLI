package com.apex.cli

object APP {
  def main(args: Array[String]): Unit = {
    var quit = false
    while (!quit) {
      quit = !Command.execute(Console.in.readLine())
    }
  }
}
