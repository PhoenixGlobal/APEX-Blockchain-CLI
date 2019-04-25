package com.apex.cli

import java.io.{ByteArrayInputStream, DataInputStream}
import java.nio.file.{Files, Paths}

import com.apex.core.{Transaction, TransactionType}
import com.apex.crypto.{BinaryData, Crypto, Ecdsa, FixedNumber, UInt160, UInt256, fromHexString}
import scala.io.Source

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
    new BroadcastCommand,
    new SendCommand,
    new RawTxCommand
  )
}

class SendCommand extends Command {
  override val cmd = "send"
  override val description = "Transfer tokens."

  override val paramList: ParameterList = ParameterList.create(
    new NicknameParameter("from", "from",
      "The account where the asset come from. Omit it if you want to send your tokens to the default account in the active wallet.",
      true),
    new StringParameter("to", "to", "The account where the asset come to"),
    new AmountParameter("amount", "amount", "The amount of the asset to be transfer."),
    new GasParameter("gasLimit", "gasLimit", "Maximum number of gas this transactions/contract is willing to pay."),
    new GasPriceParameter("gasPrice", "gasPrice", "The price of gas that the transaction / contract is willing to pay."),
    new IntParameter("nonce", "nonce", "The nonce value of this transaction, optional parameter, used to determine the order of the transaction on the block chain.",
      true)
  )

  override def execute(params: List[String]): Result = {
    try {
      val checkResult = Account.checkWalletStatus
      if (!checkResult.isEmpty) InvalidParams(checkResult)
      else {
        // 赋值from昵称
        var from = paramList.params(0).asInstanceOf[NicknameParameter].value

        // 根据昵称获取转账地址
        if (null == from) from = WalletCache.getActivityWallet().implyAccount

        val to = paramList.params(1).asInstanceOf[StringParameter].value
        var toAdress = ""
        if (to.length == 35) toAdress = to
        else if (Account.checkAccountStatus(to)) toAdress = Account.getAccount(to).address

        if (!Account.checkAccountStatus(from)) InvalidParams("from account not exists, please type a different one")
        else if (toAdress.isEmpty) InvalidParams("to account not exists, please type a different one")
        else if (Account.getAccount(from).address.equals(toAdress)) InvalidParams("same address, please type a different one")
        else if (Ecdsa.PublicKeyHash.fromAddress(toAdress) == None) InvalidParams("error to address, please type a different one")
        else {
          WalletCache.reActWallet
          val amount = paramList.params(2).asInstanceOf[AmountParameter].value
          val gasLimit = paramList.params(3).asInstanceOf[GasParameter].value
          val price = paramList.params(4).asInstanceOf[GasPriceParameter].value
          val gasPrice = Util.calcGasPrice(price)
          val nonce = paramList.params(5).asInstanceOf[IntParameter].value

          val privKey = Account.getAccount(from).getPrivKey()
          val account = RPC.post("showaccount", s"""{"address":"${privKey.publicKey.address}"}""")

          var nextNonce = Account.getResultNonce(account)
          val balance = Account.getResultBalance(account)

          if (nextNonce != 0 && nonce > 0 && nextNonce > nonce) InvalidParams("The nonce must be greater than the maximum value that the current address has used on the chain.")
          // 判断账户余额是否充足
          else if (BigDecimal.apply(balance) < amount) InvalidParams("insufficient account balance")
          else {

            if (nonce > 0) nextNonce = nonce.longValue()

            val tx = Util.buildTx(TransactionType.Transfer, from, Ecdsa.PublicKeyHash.fromAddress(toAdress).get, FixedNumber.fromDecimal(amount),
              BinaryData.empty, true, nextNonce, gasPrice, BigInt(gasLimit))
            val result = Util.sendTx(tx)

            if (ChainCommand.checkTxSucceed(result)) Success("Transaction broadcast success, the transaction hash is " + tx.id())
            else ChainCommand.returnTxFail(result)
          }

        }
      }
    } catch {
      case e: Throwable => Error(e)
    }
  }
}

class BroadcastCommand extends Command {
  override val cmd = "broadcast"
  override val description = "Broadcast original transaction information."

  override val paramList: ParameterList = ParameterList.create(
    new StringParameter("data", "data", "Original transaction information calculated using the signature algorithm.")
  )

  override def execute(params: List[String]): Result = {
    try {
      val data = paramList.params(0).asInstanceOf[StringParameter].value
      val dataContent = Util.readFile(data)

      if (dataContent.isEmpty) InvalidParams("data is empty, please type a different one")
      else {

        val binaryData = BinaryData(dataContent)
        val is = new DataInputStream(new ByteArrayInputStream(binaryData))
        val tx = Transaction.deserialize(is)

        if (tx.verifySignature()) {
          val rpcResult = Util.sendTx(tx)
          if (ChainCommand.checkTxSucceed(rpcResult)) Success("Transaction broadcast success, the transaction hash is " + tx.id())
          else ChainCommand.returnTxFail(rpcResult)
        } else Success("There was an error in the original transaction information that could not be resolved.")
      }

    } catch {
      case e: Throwable => Error(e)
    }
  }
}

class RawTxCommand extends Command {
  override val cmd = "rawTx"
  override val description = ""

  override val paramList: ParameterList = ParameterList.create(
    new NicknameParameter("from", "from",
      "The account where the asset come from. Omit it if you want to send your tokens to the default account in the active wallet.",
      true),
    new StringParameter("to", "to", "The account where the asset come to"),
    new AmountParameter("amount", "amount", "The amount of the asset to be transfer."),
    new GasParameter("gasLimit", "gasLimit", "Maximum number of gas this transactions/contract is willing to pay."),
    new GasPriceParameter("gasPrice", "gasPrice", "The price of gas that the transaction / contract is willing to pay.")
  )

  override def execute(params: List[String]): Result = {
    try {
      val checkResult = Account.checkWalletStatus
      if (!checkResult.isEmpty) InvalidParams(checkResult)
      else {
        // 赋值from昵称
        var from = paramList.params(0).asInstanceOf[NicknameParameter].value

        // 根据昵称获取转账地址
        if (null == from) from = WalletCache.getActivityWallet().implyAccount

        val to = paramList.params(1).asInstanceOf[StringParameter].value
        var toAdress = ""
        if (to.length == 35) toAdress = to
        else if (Account.checkAccountStatus(to)) toAdress = Account.getAccount(to).address

        if (!Account.checkAccountStatus(from)) InvalidParams("from account not exists, please type a different one")
        else if (toAdress.isEmpty) InvalidParams("to account not exists, please type a different one")
        else if (Account.getAccount(from).address.equals(toAdress)) InvalidParams("same address, please type a different one")
        else if (Ecdsa.PublicKeyHash.fromAddress(toAdress) == None) InvalidParams("error to address, please type a different one")
        else {
          WalletCache.reActWallet
          val amount = paramList.params(2).asInstanceOf[AmountParameter].value
          val gasLimit = paramList.params(3).asInstanceOf[GasParameter].value
          val price = paramList.params(4).asInstanceOf[GasPriceParameter].value
          val gasPrice = Util.calcGasPrice(price)

          val privKey = Account.getAccount(from).getPrivKey()
          val account = RPC.post("showaccount", s"""{"address":"${privKey.publicKey.address}"}""")

          val nextNonce = Account.getResultNonce(account)
          val balance = Account.getResultBalance(account)

          if (BigDecimal.apply(balance) < amount) InvalidParams("insufficient account balance")
          else {

            val tx = Util.buildTx(TransactionType.Transfer, from, Ecdsa.PublicKeyHash.fromAddress(toAdress).get, FixedNumber.fromDecimal(amount),
              BinaryData.empty, true, nextNonce, gasPrice, BigInt(gasLimit))

            Success(BinaryData(tx.toBytes).toString)
          }

        }
      }
    } catch {
      case e: Throwable => Error(e)
    }
  }
}
