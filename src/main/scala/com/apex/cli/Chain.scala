package com.apex.cli

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
    new TransactionCommand
  )
}

class StatusCommand extends Command {
  override val cmd = "status"
  override val description = "Show the status of block chain"

  override def execute(params: List[String]): Result = {
    try {
      val rpcResult = RPC.post("getLatesBlockInfo", paramList.toJson())
      WalletCache.reActWallet
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

      val result = RPC.post("getblock", data)
      ChainCommand.checkRes(result)
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
      val checkResult = Account.checkWalletStatus
      if (!checkResult.isEmpty) InvalidParams(checkResult)
      else {
        val id = paramList.params(0).asInstanceOf[StringParameter].value
        val rpcResult = RPC.post("getContract", s"""{"id":"${id}"}""")
        if (ChainCommand.checkSucceed(rpcResult)) {

          if (ChainCommand.checkNotNull(rpcResult)) {
            ChainCommand.returnSuccess(rpcResult)
          } else Success("No transaction information was queried")

        } else ChainCommand.returnFail(rpcResult)
      }
    } catch {
      case e: Throwable => Error(e)
    }
  }
}

object ChainCommand {

  def getStrRes(rpcRes: JsValue): String = {
    (rpcRes \ "result").as[String]
  }

  def getBooleanRes(rpcRes: JsValue): Boolean = {
    getStrRes(rpcRes).toBoolean
  }

  def checkSucceed(rpcRes: JsValue): Boolean = {

    if ((rpcRes \ "succeed").as[Boolean]) {
      true
    } else false
  }

  def checkNotNull(rpcRes: JsValue): Boolean = {
    val result = ChainCommand.getStrRes(rpcRes)

    if ((rpcRes \ "succeed").as[Boolean] && "null" != result) {
      true
    } else false
  }

  def checkRes(rpcRes: JsValue): Result = {

    if ((rpcRes \ "succeed").as[Boolean]) {
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

  def regJson(json: Option[Any]) = json match {
    case Some(map: Map[String, Any]) => map
  }
}
