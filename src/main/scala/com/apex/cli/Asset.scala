package com.apex.cli

import com.apex.core.{Transaction, TransactionType}
import com.apex.crypto.{BinaryData, Ecdsa, FixedNumber, UInt160, UInt256}

/*
 * Copyright  2018 APEX Technologies.Co.Ltd. All rights reserved.
 *
 * FileName: Asset.scala
 *
 * @author: whitney.wei@chinapex.com: 19-01-10 @version: 1.0
 */
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
          val to = paramList.params(1).asInstanceOf[NicknameParameter].value
          // 获取用户地址
          if (Account.checkAccountStatus(to)) toAdress = Account.getAccount(to).address
        }else toAdress = paramList.params(1).asInstanceOf[AddressParameter].value

        if(!Account.checkAccountStatus(from)) InvalidParams("from account not exists, please type a different one")
        else if(toAdress.isEmpty) InvalidParams("to account not exists, please type a different one")
        else if(Account.getAccount(from).address.equals(toAdress)) InvalidParams("same address, please type a different one")
        else if(Ecdsa.PublicKeyHash.fromAddress(toAdress) == None) InvalidParams("error to address, please type a different one")
        else{
          val amount = paramList.params(2).asInstanceOf[AmountParameter].value

          val privKey = Account.getAccount(from).getPrivKey()
          val account = RPC.post("showaccount", s"""{"address":"${privKey.publicKey.address}"}""")

          val nextNonce = Account.getResultNonce(account)
          // 获取账户余额
          val balance = Account.getResultBalance(account)
          // 判断账户余额是否充足
          if(BigDecimal.apply(balance) < amount) InvalidParams("insufficient account balance")
          else{

            val tx = AssetCommand.buildTx(TransactionType.Transfer, from, Ecdsa.PublicKeyHash.fromAddress(toAdress).get, FixedNumber.fromDecimal(amount),
              BinaryData.empty, true, nextNonce)
            val result = AssetCommand.sendTx(tx)

            WalletCache.reActWallet
            if(ChainCommand.checkSucceed(result)) Success("execute succeed, the transaction hash is "+tx.id())
            else ChainCommand.checkRes(result)
          }

        }
      }
    } catch {
      case e: Throwable => Error(e)
    }
  }
}

object AssetCommand{

  def buildTx(txType:TransactionType.Value, from:String, to:UInt160, amount : FixedNumber,
              data:Array[Byte], checkedAccount:Boolean = false,  nonce:Long = 0) = {

    val privKey = Account.getAccount(from).getPrivKey()
    var nextNonce: Long = nonce

    if(!checkedAccount){
      val account = RPC.post("showaccount", s"""{"address":"${privKey.publicKey.address}"}""")
      nextNonce = Account.getResultNonce(account)
    }

    val tx = new Transaction(txType,
      privKey.publicKey.pubKeyHash,
      to,
      amount,
      nextNonce,
      data,
      FixedNumber.Zero,
      BigInt(7000000),
      BinaryData.empty)
    tx.sign(privKey)

    tx
  }

  def sendTx(tx:Transaction) = {

    val txRawData = BinaryData(tx.toBytes)
    val rawTx: String = "{\"rawTx\":\"" + txRawData.toString + "\"}"
    val result = RPC.post("sendrawtransaction", rawTx)
    result
  }

}

