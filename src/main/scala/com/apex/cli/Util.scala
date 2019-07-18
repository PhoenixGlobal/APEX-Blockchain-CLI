package com.apex.cli

import java.nio.file.{Files, Paths}

import com.apex.core.{Transaction, TransactionType}
import com.apex.crypto.{BinaryData, FixedNumber, UInt160}

import scala.io.Source

object Util {
  def buildTx(txType: TransactionType.Value, from: String, to: UInt160, amount: FixedNumber, data: Array[Byte],
              checkedAccount: Boolean = false, nonce: Long = 0, gasPrice: FixedNumber = FixedNumber.Zero, gasLimit: BigInt = 50000) = {

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
    val unit = price.replaceAll("^\\d+", "").toLowerCase
    val priceNum = BigDecimal.apply(price.substring(0, price.length - unit.length))

    val fPrice = FixedNumber.fromDecimal(priceNum)
    unit match {
      case "" =>gasPrice = FixedNumber.P * fPrice       //default unit
      case "p" => gasPrice = FixedNumber.P * fPrice
      case "k" => gasPrice = FixedNumber.KP * fPrice
      case "m" => gasPrice = FixedNumber.MP * fPrice
      case "g" => gasPrice = FixedNumber.GP * fPrice
      case "kg" => gasPrice = FixedNumber.KGP * fPrice
      case "mg" => gasPrice = FixedNumber.MGP * fPrice
      case "c" => gasPrice = FixedNumber.CPX * fPrice
      case _ =>
        throw  new RuntimeException("wrong gasPrice unit. only support(p|k|m|g|kg|mg|c)")
    }
    gasPrice / FixedNumber.One
  }
}
