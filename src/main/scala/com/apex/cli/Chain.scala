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
    try{
      val result = RPC.post("getblockheight", paramList.toJson())
      WalletCache.reActWallet
      ChainCommand.checkRes(result)
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
      "The height of block. Use either this param or \"id\", If both give, the front one make sense.", true,true),
    new PrivKeyParameter("hash", "hash",
      "The hash of block. Use either this param or \"id\", If both give, the front one make sense.",true,true)

  )

  override def execute(params: List[String]): Result = {

    try{
      val height = paramList.params(0).asInstanceOf[IntParameter].value

      var data:String =""
      if(height != null)
        data = JsObject(
          mutable.HashMap(paramList.params(0). asInstanceOf[IntParameter].name.toString -> paramList.params(0).asInstanceOf[IntParameter].toJson)).toString()
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
  override val cmd = "transaction"
  override val description = "how data of the transaction"

  override val paramList: ParameterList = ParameterList.create(
    new StringParameter("hash", "hash","The hash of transaction.")
  )

  override def execute(params: List[String]): Result = {
    try{
      val checkResult = Account.checkWalletStatus
      if (!checkResult.isEmpty) InvalidParams(checkResult)
      else {
        val id = paramList.params(0).asInstanceOf[StringParameter].value
        val rpcResult = RPC.post("getContract", s"""{"id":"${id}"}""")
        ChainCommand.checkRes(rpcResult)
      }
    } catch {
      case e: Throwable => Error(e)
    }
  }
}

object ChainCommand{

  def checkSucceed(res:JsValue): Boolean ={

    if((res \ "succeed").as[Boolean]){
      true
    }else false
  }

    def checkRes(res:JsValue): Result ={

      if((res \ "succeed").as[Boolean]){
        // 返回结果值
        val result = (res \ "result").as[String]
        Success(Json prettyPrint Json.parse(result))
      }else{
        // 返回状态码，和message
        val status = (res \ "status").as[Int]
        val message = (res \ "message").as[String]
        Success("execute failed, status is "+status+", message is "+message)
      }
    }

  def regJson(json: Option[Any]) = json match {
    case Some(map: Map[String, Any]) => map
  }
}
