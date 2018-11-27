package com.apex.cli.core

class ChainCommand extends NewCompositeCommand {
  override val cmd: String = "chain"
  override val description: String = "Command Line Interface to the block chain, omit it and type the sub command directly is legal."

  override val subCommands: Seq[NewCommand] = Seq(

  )
}
class CirculateCommand extends SendCommand {
  override val cmd = "circulate"
  override val description = "Transfer tokens between accounts within current wallet. "
}

class SendCommand extends NewCommand {
  override val cmd = "send"
  override val description = "Transfer tokens."
  override val sys: Boolean = true

  override val paramList: NewParameterList = NewParameterList.create(
    new NewAddressParameter("from", "from"),
    new NewAddressParameter("to", "to"),
    new NewAmountParameter("amount", "amount")
  )

  override def execute(params: List[String]): NewResult = {null}
}

