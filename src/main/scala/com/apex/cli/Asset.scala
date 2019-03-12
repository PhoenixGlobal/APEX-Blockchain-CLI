package com.apex.cli

import java.io.{ByteArrayInputStream, DataInputStream}
import java.nio.file.{Files, Paths}

import com.apex.core.{Transaction, TransactionType}
import com.apex.crypto.Ecdsa.PublicKeyHash
import com.apex.crypto.{BinaryData, Crypto, Ecdsa, FixedNumber, UInt160, UInt256, fromHexString}
import org.bouncycastle.util.encoders.Hex

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
    new SendCommand
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
    new GasPriceParameter("nonce", "nonce", "The nonce value of this transaction, optional parameter, used to determine the order of the transaction on the block chain.",
      true)
  )

  override def execute(params: List[String]): Result = {
    try {
      val checkResult = Account.checkWalletStatus
      if (!checkResult.isEmpty) InvalidParams(checkResult)
      else {
        // 赋值from昵称
        var from = WalletCache.getActivityWallet().implyAccount
        // 根据昵称获取转账地址
        if (params.size / 2 == paramList.params.size) from = paramList.params(0).asInstanceOf[NicknameParameter].value

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
          val gasPrice = AssetCommand.calcGasPrice(price)

          val privKey = Account.getAccount(from).getPrivKey()
          val account = RPC.post("showaccount", s"""{"address":"${privKey.publicKey.address}"}""")

          val nextNonce = Account.getResultNonce(account)
          // 获取账户余额
          val balance = Account.getResultBalance(account)
          // 判断账户余额是否充足
          if (BigDecimal.apply(balance) < amount) InvalidParams("insufficient account balance")
          else {

            val tx = AssetCommand.buildTx(TransactionType.Transfer, from, Ecdsa.PublicKeyHash.fromAddress(toAdress).get, FixedNumber.fromDecimal(amount),
              BinaryData.empty, true, nextNonce, gasPrice, BigInt(gasLimit))
            val result = AssetCommand.sendTx(tx)

            if (ChainCommand.checkSucceed(result)) Success("execute succeed, the transaction hash is " + tx.id())
            else ChainCommand.returnFail(result)
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
    new StringParameter("data", "data", "")
  )

  override def execute(params: List[String]): Result = {
    try {
        val data = paramList.params(0).asInstanceOf[StringParameter].value
        val dataContent = AssetCommand.readFile(data)

        WalletCache.reActWallet
        if (dataContent.isEmpty) InvalidParams("data is empty, please type a different one")
        else {

          val binaryData = BinaryData(dataContent)
          val is = new DataInputStream(new ByteArrayInputStream(binaryData))
          val tx = Transaction.deserialize(is)

          if (tx.verifySignature()) {
            val result = AssetCommand.sendTx(tx)
            if (ChainCommand.checkSucceed(result)) Success("execute succeed, the transaction hash is " + tx.id())
            else ChainCommand.returnFail(result)
          } else Success("There was an error in the original transaction information that could not be resolved.")
        }

    } catch {
      case e: Throwable => Error(e)
    }
  }
}

object AssetCommand {

  def buildTx(txType: TransactionType.Value, from: String, to: UInt160, amount: FixedNumber, data: Array[Byte],
              checkedAccount: Boolean = false, nonce: Long = 0, gasPrice: FixedNumber = FixedNumber.Zero, gasLimit: BigInt = 7000000) = {

    val privKey = Account.getAccount(from).getPrivKey()
    var nextNonce: Long = nonce

    if (!checkedAccount) {
      nextNonce = Account.getNonce(privKey.publicKey.address)
    }

    val tx = new Transaction(txType,
      privKey.publicKey.pubKeyHash,
      to,
      amount,
      nextNonce,
      data,
      gasPrice,
      gasLimit,
      BinaryData.empty)
    tx.sign(privKey)

    tx
  }

  def sendTx(tx: Transaction) = {

    val txRawData = BinaryData(tx.toBytes)
    val rawTx: String = "{\"rawTx\":\"" + txRawData.toString + "\"}"
    val result = RPC.post("sendrawtransaction", rawTx)
    result
  }

  def readFile(fileName: String): String = {
    var content = ""
    if (Files.exists(Paths.get(fileName))) {
      val file = Source.fromFile(fileName)
      content = file.getLines.mkString
      file.close()
    }
    content
  }

  def calcGasPrice(price: String): FixedNumber = {
    var gasPrice = FixedNumber.Zero
    // 获取最后一位
    val unit = price.toLowerCase.substring(price.length - 1).charAt(0)
    val priceNum = price.substring(0, price.length - 1).toLong
    unit match {
      case 'p' => gasPrice = FixedNumber(BigInt.long2bigInt(priceNum))
      case 'k' => gasPrice = FixedNumber(BigInt.long2bigInt((Math.pow(10, 9) * priceNum).longValue())) // 10的3次幂 p
      case 'm' => gasPrice = FixedNumber(BigInt.long2bigInt((Math.pow(10, 12) * priceNum).longValue())) // 10的6次幂 p
      case 'g' => gasPrice = FixedNumber(BigInt.long2bigInt((Math.pow(10, 15) * priceNum).longValue())) // 10的9次幂 p
      case 'c' => gasPrice = FixedNumber(BigInt.long2bigInt((Math.pow(10, 18) * priceNum).longValue())) // 10的12次幂 p
    }
    gasPrice
  }
}

