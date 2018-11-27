package com.apex.cli.core

class SysCommand extends NewCompositeCommand {
  override val cmd: String = "sys"
  override val description: String = "Command Line Interface to the system, omit it and type the sub command directly is legal."

  override val subCommands: Seq[NewCommand] = Seq(
    new NewVersionC
  )
}

class NewHelpC extends NewCommand {
  override val cmd: String = "help"
  override val description: String = "help"
  override val sys: Boolean = true

  override def execute(params: List[String]): NewResult = {

    NewHelp(NewCommand.helpMessage(NewCommand.all))
  }
}

class NewVersionC extends NewVerC {
  override val cmd = "version"
}

class NewVerC extends NewCommand {
  override val cmd = "ver"
  override val description = "Version information"
  override val sys: Boolean = true

  override def execute(params: List[String]): NewResult = {null}
}

class NewExitC extends NewCommand {
  override val cmd = "exit"
  override val description = "exit"
  override val sys: Boolean = true

  override def execute(params: List[String]): NewResult = new NewQuit
}
