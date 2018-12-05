package com.apex.cli

class ChainCommand extends CompositeCommand {
  override val cmd: String = "chain"
  override val description: String = "Command Line Interface to the block chain, omit it and type the sub command directly is legal."

  override val subCommands: Seq[Command] = Seq(
    new StatusCommand,
    new BlockCommand,
    new TransactionCommand
  )
}

class StatusCommand extends Command {
  override val cmd = "status"
  override val description = "Show the status of block chain"
  override val sys: Boolean = true

  override def execute(params: List[String]): Result = {null}
}

class BlockCommand extends Command {
  override val cmd = "block"
  override val description = "Show data of the block"
  override val sys: Boolean = true


  override val paramList: ParameterList = ParameterList.create(
    new IntParameter("height", "height", true,true),
    new IntParameter("id", "id", true,true)
  )

  override def execute(params: List[String]): Result = {null}
}

class TransactionCommand extends Command {
  override val cmd = "transaction"
  override val description = "how data of the transaction"

  override val paramList: ParameterList = ParameterList.create(
    new IntParameter("id", "id")
  )

  override def execute(params: List[String]): Result = {null}
}
