package com.apex.cli.core

class ChainCommand extends NewCompositeCommand {
  override val cmd: String = "chain"
  override val description: String = "Command Line Interface to the block chain, omit it and type the sub command directly is legal."

  override val subCommands: Seq[NewCommand] = Seq(

  )
}
