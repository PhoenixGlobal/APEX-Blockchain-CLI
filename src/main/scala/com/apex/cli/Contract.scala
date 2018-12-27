package com.apex.cli

import spray.json.JsNull
import com.apex.core.{Transaction, TransactionType}
import com.apex.crypto.{BinaryData, Ecdsa, Fixed8, UInt160, UInt256}
import play.api.libs.json.Json

class ContractCommand extends CompositeCommand {

  override val cmd: String = "contract"
  override val description: String = "Operate smart contract"
  override val composite: Boolean = true

  override val subCommands: Seq[Command] = Seq(
    new ACommand,
    new BCommand
  )
}

class ACommand extends Command {
  override val cmd = "a"
  override val description = ""

  override val paramList: ParameterList = ParameterList.create(
    new NicknameParameter("from", "from",
      "",true),
    new StringParameter("data", "data","")
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

        val data = paramList.params(1).asInstanceOf[StringParameter].value

        if(!Account.checkAccountStatus(from)) InvalidParams("from account not exists, please type a different one")
        else{
          val privKey = Account.getAccount(from).getPrivKey()

          val account = RPC.post("showaccount", s"""{"address":"${privKey.publicKey.address}"}""")

          var nextNonce: Long = 0
          if (account != JsNull) {
            nextNonce = (account \ "nextNonce").as[Long]
          }

          val tx = new Transaction(TransactionType.Transfer, privKey.publicKey, UInt160.Zero,"", Fixed8.Zero,
            UInt256.Zero, nextNonce, BinaryData.apply(data),BinaryData.empty)
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


class BCommand extends Command {
  override val cmd = "b"
  override val description = ""

  override val paramList: ParameterList = ParameterList.create(
    new NicknameParameter("from", "from",
      "",true),
    new AddressParameter("to", "to",
      "",true),
    new StringParameter("abi", "abi",""),
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

        val to = paramList.params(1).asInstanceOf[AddressParameter].value
        val abi = paramList.params(2).asInstanceOf[StringParameter].value

        if(!Account.checkAccountStatus(from)) InvalidParams("from account not exists, please type a different one")
        else{
          val privKey = Account.getAccount(from).getPrivKey()

          val account = RPC.post("showaccount", s"""{"address":"${privKey.publicKey.address}"}""")

          var nextNonce: Long = 0
          if (account != JsNull) {
            nextNonce = (account \ "nextNonce").as[Long]
          }

          val tx = new Transaction(TransactionType.Transfer,
            privKey.publicKey,
            Ecdsa.PublicKeyHash.fromAddress(to).get,
            "",
            Fixed8.Zero,
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

