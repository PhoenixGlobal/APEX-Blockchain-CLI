package com.apex.cli

import com.apex.core.{Transaction, TransactionType}
import com.apex.crypto.{BinaryData, Ecdsa, Fixed8, UInt256}
import play.api.libs.json.{JsNull, Json}

class AssetCommand extends CompositeCommand {
  override val cmd: String = "asset"
  override val description: String = "Interface to operate your funds,  omit it and type the sub command directly is legal."
  override val composite: Boolean = true

  override val subCommands: Seq[Command] = Seq(
    new CirculateCommand,
    new SendCommand
  )
}

class CirculateCommand extends SendCommand {
  override val cmd = "circulate"
  override val description = "Transfer tokens between accounts within current wallet. "
  override val sys: Boolean = true

  override val paramList: ParameterList = ParameterList.create(
    new NicknameParameter("from", "from",
      "The account where the asset come from. Omit it if you want to send your tokens to the default account in the active wallet.",
      true),
    new NicknameParameter("to", "to","The account where the asset come to"),
    new AmountParameter("amount", "amount","The amount of the asset to be transfer.")
  )
}

class SendCommand extends Command {
  override val cmd = "send"
  override val description = "Transfer tokens."
  override val sys: Boolean = true

  override val paramList: ParameterList = ParameterList.create(
    new NicknameParameter("from", "from",
      "The account where the asset come from. Omit it if you want to send your tokens to the default account in the active wallet.",
      true),
    new AddressParameter("to", "to","The account where the asset come to"),
    new AmountParameter("amount", "amount","The amount of the asset to be transfer.")
  )

  override def execute(params: List[String]): Result = {
    try{
      val checkResult = Account.checkWalletStatus
      if(!checkResult.isEmpty) InvalidParams(checkResult)
      else {
        // 赋值from昵称
        var from = WalletCache.getActivityWallet().implyAccount
        // 根据昵称获取转账地址
        if(params.size/2 == paramList.params.size)  from = paramList.params(0).asInstanceOf[NicknameParameter].value

        // 赋值to收账地址
        var toAdress = ""
        if(this.cmd.equals("circulate")) {
          var to = paramList.params(1).asInstanceOf[NicknameParameter].value
          // 获取用户地址
          if (Account.checkAccountStatus(to)) toAdress = Account.getAccount(to).address
        }else toAdress = paramList.params(1).asInstanceOf[AddressParameter].value

        if(!Account.checkAccountStatus(from)) InvalidParams("from account not exists, please type a different one")
        else if(toAdress.isEmpty) InvalidParams("to account not exists, please type a different one")
        else if(Account.getAccount(from).address.equals(toAdress)) InvalidParams("same address, please type a different one")
        else{
          val amount = paramList.params(2).asInstanceOf[AmountParameter].value
          val privKey = Account.getAccount(from).getPrivKey()

          val account = RPC.post("showaccount", s"""{"address":"${privKey.publicKey.address}"}""")

          var nextNonce: Long = 0
          if (account != JsNull) {
            nextNonce = (account \ "nextNonce").as[Long]
          }

          val tx = new Transaction(TransactionType.Transfer,
            privKey.publicKey,
            Ecdsa.PublicKeyHash.fromAddress(toAdress).get,
            "",
            Fixed8.fromDecimal(amount),
            UInt256.Zero,
            nextNonce,
            BinaryData.empty,
            BinaryData.empty)

          tx.sign(privKey)

          val txRawData = BinaryData(tx.toBytes)
          val rawTx: String = "{\"rawTx\":\""  + txRawData.toString  + "\"}"
          val result = RPC.post("sendrawtransaction", rawTx)
          WalletCache.reActWallet
          Success(Json prettyPrint result)
        }
      }
    } catch {
      case e: Throwable => Error(e)
    }
  }
}

