package com.apex.cli

import play.api.libs.json.{JsObject, Json}
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
    new ResetGasLimitCommand,
    new GasLimitCommand
  )
}

class StatusCommand extends Command {
  override val cmd = "status"
  override val description = "Show the status of block chain"

  override def execute(params: List[String]): Result = {
    try{
      val result = RPC.post("getblockheight", paramList.toJson())
      WalletCache.reActWallet
      Success(Json prettyPrint result)
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
    new PrivKeyParameter("id", "id",
      "The id of block. Use either this param or \"id\", If both give, the front one make sense.",true,true)

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
      Success(Json prettyPrint result)
    } catch {
      case e: Throwable => Error(e)
    }
  }
}

class TransactionCommand extends Command {
  override val cmd = "transaction"
  override val description = "how data of the transaction"

  override val paramList: ParameterList = ParameterList.create(
    new StringParameter("id", "id","The id of transaction.")
  )

  override def execute(params: List[String]): Result = {
    try{
      val result = RPC.post("gettx", paramList.toJson())
      WalletCache.reActWallet
      Success(Json prettyPrint result)
    } catch {
      case e: Throwable => Error(e)
    }
  }
}

class ResetGasLimitCommand  extends Command {
  override val cmd = "resetGasLimit"
  override val description = "Modify the gas limit of the production node"

  override val paramList: ParameterList = ParameterList.create(
    new IntParameter("gasLimit", "gasLimit","gasLimit")
  )

  override def execute(params: List[String]): Result = {
    try{
      var gasLimit = paramList.params(0).asInstanceOf[IntParameter].value
      val result = RPC.post("setGasLimit", s"""{"gasLimit":"${gasLimit}"}""", RPC.secretRpcUrl)
      Success(Json prettyPrint result)
    }catch {
      case e: Throwable => Error(e)
    }
  }
}

class GasLimitCommand  extends Command {
  override val cmd = "gasLimit"
  override val description = "Query the gas limit of the production node"
  override def execute(params: List[String]): Result = {

    try{
      val result1 = RPC.post("getGasLimit", paramList.toJson(), RPC.secretRpcUrl)
      Success(Json prettyPrint result1)
    }catch {
      case e: Throwable => Error(e)
    }
  }
}
