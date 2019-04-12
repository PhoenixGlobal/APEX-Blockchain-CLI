package com.apex.cli

import com.apex.crypto.FixedNumber
import play.api.libs.json.{JsObject, JsValue, Json}
import scala.collection.mutable

/*
 * Copyright  2018 APEX Technologies.Co.Ltd. All rights reserved.
 *
 * FileName: Asset.scala
 *
 * @author: whitney.wei@chinapex.com: 18-11-10 @version: 1.0
 */
class ChainCommand extends CompositeCommand {
  override val cmd: String = "chain"
  override val description: String = "Command Line Interface to the block chain, omit it and type the sub command directly is legal."
  override val composite: Boolean = true

  override val subCommands: Seq[Command] = Seq(
    new StatusCommand,
    new BlockCommand,
    new TransactionCommand,
    new GasCommand,
    new ChainAccountCommand
  )
}

class StatusCommand extends Command {
  override val cmd = "status"
  override val description = "Show the status of block chain"

  override def execute(params: List[String]): Result = {
    try {
      val rpcResult = RPC.post("getLatesBlockInfo", paramList.toJson())
      if (ChainCommand.checkSucceed(rpcResult)) {
        val result = ChainCommand.getStrRes(rpcResult)
        // 获取用户index和id
        val hash = (Json.parse(result) \ "hash").as[String]
        val height = (Json.parse(result) \ "index").as[Long]
        Success("The latest block height is " + height + ", the block hash is " + hash)
      } else ChainCommand.returnFail(rpcResult)
    } catch {
      case e: Throwable => Error(e)
    }
  }
}

class BlockCommand extends Command {
  override val cmd = "block"
  override val description = "Show data of the block"

  override val paramList: ParameterList = ParameterList.create(
    new IntParameter("height", "height",
      "The height of block. Use either this param or \"id\", If both give, the front one make sense.", true, true),
    new PrivKeyParameter("hash", "hash",
      "The hash of block. Use either this param or \"id\", If both give, the front one make sense.", true, true)
  )

  override def execute(params: List[String]): Result = {

    try {
      val height = paramList.params(0).asInstanceOf[IntParameter].value
      var data: String = ""
      if (height != null)
        data = JsObject(
          mutable.HashMap(paramList.params(0).asInstanceOf[IntParameter].name.toString -> paramList.params(0).asInstanceOf[IntParameter].toJson)).toString()
      else
        data = JsObject(
          mutable.HashMap(paramList.params(1).asInstanceOf[PrivKeyParameter].name.toString -> paramList.params(1).asInstanceOf[PrivKeyParameter].toJson)).toString()

      val rpcResult = RPC.post("getblock", data)

      if (!ChainCommand.checkSucceed(rpcResult)) {
        ChainCommand.returnFail(rpcResult)
      } else if (ChainCommand.checkNotNull(rpcResult)) {
        ChainCommand.returnSuccess(rpcResult)
      } else Success("This block was not queried.")

    } catch {
      case e: Throwable => Error(e)
    }
  }
}

class ChainAccountCommand extends Command {
  override val cmd = "account"
  override val description = "Show data of a account"

  override val paramList: ParameterList = ParameterList.create(
    new AddressParameter("address", "address",
      "The address of account.", true, true)
  )

  override def execute(params: List[String]): Result = {
    try {
      val addr = paramList.params(0).asInstanceOf[AddressParameter].value
      var data: String = ""
      if (addr != null)
        data = JsObject(
          mutable.HashMap("address" -> paramList.params(0).asInstanceOf[AddressParameter].toJson)).toString()

      val rpcResult = RPC.post("showaccount", data)

      if (!ChainCommand.checkSucceed(rpcResult)) {
        ChainCommand.returnFail(rpcResult)
      } else if (ChainCommand.checkNotNull(rpcResult)) {
        ChainCommand.returnSuccess(rpcResult)
      } else Success("This account was not queried.")

    } catch {
      case e: Throwable => Error(e)
    }
  }
}

class TransactionCommand extends Command {
  override val cmd = "tx"
  override val description = "how data of the transaction"

  override val paramList: ParameterList = ParameterList.create(
    new StringParameter("hash", "hash", "The hash of transaction.")
  )

  override def execute(params: List[String]): Result = {
    try {
      val id = paramList.params(0).asInstanceOf[StringParameter].value
      val rpcResult = RPC.post("getContract", s"""{"id":"${id}"}""")

      if (!ChainCommand.checkSucceed(rpcResult)) {
        ChainCommand.returnFail(rpcResult)
      } else if (ChainCommand.checkNotNull(rpcResult)) {
        ChainCommand.returnSuccess(rpcResult)
      } else Success("No transaction information was queried")

    } catch {
      case e: Throwable => Error(e)
    }
  }
}

class GasCommand extends Command {
  override val cmd = "gas"
  override val description = "Query the average gas price of the latest 1000 blocks on the block chain."

  override def execute(params: List[String]): Result = {
    try {
      val rpcResult = RPC.post("getAverageGasPrice", paramList.toJson())

      if (ChainCommand.checkSucceed(rpcResult)) {
        val result = ChainCommand.getStrRes(rpcResult)

        Success("The average gas price is " + fixedNumberToStr(BigDecimal.apply(result)))
      } else ChainCommand.returnFail(rpcResult)
    } catch {
      case e: Throwable => Error(e)
    }
  }

  def fixedNumberToStr(price: BigDecimal): String = {

    val fPrice = FixedNumber.fromDecimal(price)
    val df = new java.text.DecimalFormat("#.00");
    var strPrice = ""
    if (fPrice >= FixedNumber.CPX) {
      strPrice = price.toString + "CPX"
    } else if (fPrice >= FixedNumber.MGP) {
      strPrice = df.format(price.*(BigDecimal.apply(1000L))) + "MGP"
    } else if (fPrice >= FixedNumber.MGP) {
      strPrice = df.format(price.*(BigDecimal.apply(1000000L))) + "KGP"
    } else if (fPrice >= FixedNumber.GP) {
      strPrice = df.format(price.*(BigDecimal.apply(1000000000L))) + "GP"
    } else if (fPrice >= FixedNumber.MP) {
      strPrice = df.format(price.*(BigDecimal.apply(1000000000000L))) + "MP"
    } else if (fPrice >= FixedNumber.KP) {
      strPrice = df.format(price.*(BigDecimal.apply(1000000000000000L))) + "KP"
    } else {
      strPrice = df.format(price.*(BigDecimal.apply(1000000000000000000L))) + "P"
    }
    strPrice
  }
}

object ChainCommand {

  def getStrRes(rpcRes: JsValue): String = {
    (rpcRes \ "result").as[String]
  }

  def getBooleanRes(rpcRes: JsValue): Boolean = {
    getStrRes(rpcRes).toBoolean
  }

  def getTxBooleanRes(rpcRes: JsValue): Boolean = {
    (Json.parse(getStrRes(rpcRes)) \ "added").as[Boolean]
  }

  def checkTxSucceed(rpcRes: JsValue): Boolean = {

    if (checkSucceed(rpcRes) && getTxBooleanRes(rpcRes)) {
      true
    } else false
  }

  def checkSucceed(rpcRes: JsValue): Boolean = {

    if ((rpcRes \ "succeed").as[Boolean]) {
      true
    } else false
  }

  def checkNotNull(rpcRes: JsValue): Boolean = {
    val result = ChainCommand.getStrRes(rpcRes)

    if (checkSucceed(rpcRes) && !"null".equals(result) && !result.isEmpty) {
      true
    } else false
  }

  def checkRes(rpcRes: JsValue): Result = {

    if (checkSucceed(rpcRes)) {
      returnSuccess(rpcRes)
    } else {
      returnFail(rpcRes)
    }
  }

  def returnSuccess(rpcRes: JsValue): Result = {
    // 返回结果值
    val result = (rpcRes \ "result").as[String]
    Success(Json prettyPrint Json.parse(result))
  }

  def returnFail(rpcRes: JsValue): Result = {
    // 返回状态码，和message
    val status = (rpcRes \ "status").as[Int]
    val message = (rpcRes \ "message").as[String]
    Success("execute failed, status is " + status + ", message is " + message)
  }

  def returnTxFail(rpcRes: JsValue): Result = {
    if (!checkSucceed(rpcRes)) {
      returnFail(rpcRes)
    } else Success((Json.parse(getStrRes(rpcRes)) \ "result").as[String])
  }
}
