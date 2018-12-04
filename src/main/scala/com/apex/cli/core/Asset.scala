package com.apex.cli.core

import com.apex.core.{Transaction, TransactionType}
import com.apex.crypto.{BinaryData, Ecdsa, Fixed8, UInt256}

class AssetCommand extends NewCompositeCommand {
  override val cmd: String = "asset"
  override val description: String = "Interface to operate your funds,  omit it and type the sub command directly is legal."

  override val subCommands: Seq[NewCommand] = Seq(
    new CirculateCommand,
    new SendCommand
  )
}

class CirculateCommand extends SendCommand {
  override val cmd = "circulate"
  override val description = "Transfer tokens between accounts within current wallet. "

  override val paramList: NewParameterList = NewParameterList.create(
    new NicknameParameter("from", "from", true),
    new NicknameParameter("to", "to"),
    new NewAmountParameter("amount", "amount")
  )
}

class SendCommand extends NewCommand {
  override val cmd = "send"
  override val description = "Transfer tokens."
  override val sys: Boolean = true

  override val paramList: NewParameterList = NewParameterList.create(
    new NicknameParameter("from", "from", true),
    new NewAddressParameter("to", "to"),
    new NewAmountParameter("amount", "amount")
  )

  override def execute(params: List[String]): NewResult = {

    // 赋值from
    var from = ""
    if(params.size/2 == paramList.params.size) from = paramList.params(0).asInstanceOf[NicknameParameter].value

    // 赋值to
    var to = ""
    if(this.cmd.equals("circulate")) to = paramList.params(1).asInstanceOf[NicknameParameter].value
    else to = paramList.params(1).asInstanceOf[NewAddressParameter].value

    val amount = paramList.params(2).asInstanceOf[NewAmountParameter].value

    /*val tx = new Transaction(TransactionType.Transfer,
      privKey.publicKey,
      Ecdsa.PublicKeyHash.fromAddress(toAddress).get,
      "",
      Fixed8.fromDecimal(amount),
      UInt256.fromBytes(BinaryData(assetId)),
      nonce,
      BinaryData.empty,
      BinaryData.empty)*/

    println(from + "-" + to + "-" + amount)

    NewSuccess("send")
  }
}

