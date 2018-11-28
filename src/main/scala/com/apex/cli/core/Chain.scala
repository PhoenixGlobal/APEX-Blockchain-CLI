package com.apex.cli.core

class ChainCommand extends NewCompositeCommand {
  override val cmd: String = "chain"
  override val description: String = "Command Line Interface to the block chain, omit it and type the sub command directly is legal."

  override val subCommands: Seq[NewCommand] = Seq(
    new StatusCommand,
    new BlockCommand,
    new TransactionCommand
  )
}

class StatusCommand extends NewCommand {
  override val cmd = "status"
  override val description = "Show the status of block chain"
  override val sys: Boolean = true

  override def execute(params: List[String]): NewResult = {null}
}

class BlockCommand extends NewCommand {
  override val cmd = "block"
  override val description = "Show data of the block"
  override val sys: Boolean = true


  override val paramList: NewParameterList = NewParameterList.create(
    new NewIntParameter("height", "height", true),
    new NewIntParameter("id", "id", true  )
  )

  override def execute(params: List[String]): NewResult = {null}
}

class TransactionCommand extends NewCommand {
  override val cmd = "transaction"
  override val description = "how data of the transaction"

  override val paramList: NewParameterList = NewParameterList.create(
    new NewIntParameter("id", "id")
  )

  override def execute(params: List[String]): NewResult = {null}
}
