package com.apex.cli

/*
 * Copyright  2018 APEX Technologies.Co.Ltd. All rights reserved.
 *
 * FileName: Sys.scala
 *
 * @author: whitney.wei@chinapex.com: 18-12-10 @version: 1.0
 */
class SysCommand extends CompositeCommand {
  override val cmd: String = "sys"
  override val description: String = "Command Line Interface to the system, omit it and type the sub command directly is legal."
  override val composite: Boolean = true

  override val subCommands: Seq[Command] = Seq(
    new VersionC,
    new HelpC,
    new VerC,
    new ExitC,
    new ClearC
  )
}

class HelpC extends Command {
  override val cmd: String = "help"
  override val description: String = "help"

  override def execute(params: List[String]): Result = {

    Help(Command.helpMessage("APEX NETWORK\n", Command.all, true))
  }
}

class VersionC extends Command {
  override val cmd = "version"
  override val description = "Version information"

  override def execute(params: List[String]): Result = {
    Success("1")
  }
}

class VerC extends VersionC {
  override val cmd = "ver"
}

class ExitC extends Command {
  override val cmd = "exit"
  override val description = "exit"

  override def execute(params: List[String]): Result = new Quit
}

class ClearC extends Command {
  override val cmd = "clear"
  override val description = "Clear characters on screen"

  override def execute(params: List[String]): Result = {

    try {
      if (System.getProperty("os.name").contains("Windows")) new ProcessBuilder("cmd", "/c", "cls").inheritIO.start.waitFor
      else {
        println(System.getProperty("os.name"))
        System.out.print("\033\143")  // Runtime.getRuntime.exec("clear")
        //   System.out.print("\u001b\u0063")
      }
      Help("Welcome to CLI, type \"help\" for command list:")
    } catch {
      case e: Throwable => Error(e)
    }
  }
}
