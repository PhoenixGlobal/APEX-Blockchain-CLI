/*
 * Copyright  2018 APEX Technologies.Co.Ltd. All rights reserved.
 *
 * FileName: Command.scala
 *
 * @author: ruixiao.xiao@chinapex.com: 18-8-10 下午1:55@version: 1.0
 */

package com.apex.cli

import com.apex.core.{Transaction, TransactionType}
import com.apex.crypto.{BinaryData, Ecdsa, Fixed8, UInt256}
import play.api.libs.json.Json

trait Result

case class Error(e: Throwable) extends Result

case class Success(data: String) extends Result

case class InvalidParams(input: String) extends Result

case class UnKnown(cmd: String) extends Result

case class Help(message: String) extends Result

case class NoInput() extends Result

case class Quit() extends Result

trait Command {
  val cmd: String
  val description: String
  val paramList: ParameterList = ParameterList.empty
  val sys: Boolean = false

  def validate(params: List[String]): Boolean = {
    paramList.validate(params)
  }

  def execute(params: List[String]): Result = {
    try {
      Success(callRPC)
    } catch {
      case e: Throwable => Error(e)
    }
  }

  protected def callRPC(): String = {
    val result = RPC.post(cmd, paramList.toJson)
    Json prettyPrint result
  }
}

class ListBlock extends Command {
  override val cmd = "getblocks"
  override val description = "list block"
}

class GetBlockById extends Command {
  override val cmd = "getblock"
  override val description = "get block by id"
  override val paramList: ParameterList = ParameterList.id
}

class GetBlockByHeight extends Command {
  override val cmd = "getblock"
  override val description = "get block by height"
  override val paramList: ParameterList = ParameterList.int("height", "h")
}

class GetBlockCount extends Command {
  override val cmd = "getblockcount"
  override val description = "get block count"
  override val paramList: ParameterList = ParameterList.empty
}

class ProduceBlock extends Command {
  override val cmd = "produceblock"
  override val description = "produce block"
  override val paramList: ParameterList = ParameterList.empty
}

class GetTransaction extends Command {
  override val cmd = "gettx"
  override val description = "get transaction"
  override val paramList: ParameterList = ParameterList.id
}

class Transfer extends Command {
  override val cmd = "sendrawtransaction"
  override val description = "transfer money"
  override val paramList: ParameterList = ParameterList.create(
    new PrivKeyParameter(),
    new AddressParameter(),
    new IdParameter("assetId", "assetId"),
    new AmountParameter(),
    new IntParameter("nonce", "nonce")
  )

  override def execute(params: List[String]): Result = {
    try {
      val privKey = Ecdsa.PrivateKey(paramList.params(0).asInstanceOf[PrivKeyParameter].value)
      val toAddress = paramList.params(1).asInstanceOf[AddressParameter].value
      val assetId = paramList.params(2).asInstanceOf[IdParameter].value
      val amount = paramList.params(3).asInstanceOf[AmountParameter].value
      val nonce = paramList.params(4).asInstanceOf[IntParameter].value

      val tx = new Transaction(TransactionType.Transfer,
        privKey.publicKey.toBin,
        Ecdsa.PublicKeyHash.fromAddress(toAddress).get,
        "",
        Fixed8.fromDecimal(amount),
        UInt256.fromBytes(BinaryData(assetId)),
        nonce,
        BinaryData.empty,
        BinaryData.empty)

      tx.sign(privKey)

      val txRawData = BinaryData(tx.toBytes)
      val rawTx: String = "{\"rawTx\":\""  + txRawData.toString  + "\"}"
      val result = RPC.post(cmd, rawTx)
      Success(Json prettyPrint result)
    } catch {
      case e: Throwable => Error(e)
    }
  }
}

class ImportPrivateKey extends Command {
  override val cmd: String = "importprivkey"
  override val description: String = "import private key"
  override val paramList: ParameterList = ParameterList.str("key", "key")
}

class HelpC extends Command {
  override val cmd: String = "help"
  override val description: String = "help"
  override val sys: Boolean = true

  private var message: String = null

  override def execute(params: List[String]): Result = {
    def paddingTail(str: String, padding: Int): String = {
      str.formatted(s"%-${padding}s")
    }

    if (message == null) {
      val title = "APEX NETWORK\n"
      val column = s"${paddingTail("name", 15)} ${paddingTail("parameter", 30)} description"
      val content = Command.all.flatMap(
        p => p._2.filterNot(_.sys).map(c => {
          val cmd = if (c == p._2(0)) c.cmd else ""
          val params = s"[${c.paramList}]"
          s"${paddingTail(cmd, 15)} ${paddingTail(params, 30)} ${c.description}"
        })).mkString("\n")
      message = s"$title\n$column\n$content"
    }

    Help(message)
  }
}

class QuitC extends Command {
  override val cmd = "quit"
  override val description = "quit"
  override val sys: Boolean = true

  override def execute(params: List[String]): Result = new Quit
}

class ExitC extends Command {
  override val cmd = "exit"
  override val description = "exit"
  override val sys: Boolean = true

  override def execute(params: List[String]): Result = new Quit
}

object Command {
  def execute(command: String): Result = {
    if (!command.trim.isEmpty) {
      val list = command.trim.split("""\s+""").toList
      list match {
        case cmd :: tail if all.contains(cmd) =>
          all(cmd).find(_.validate(tail)) match {
            case Some(command) => command.execute(tail)
            case None => InvalidParams(tail.mkString(" "))
          }
        case cmd :: _ => UnKnown(cmd)
        case _ => NoInput()
      }
    } else {
      NoInput()
    }
  }

  val all = Seq(
    new ListBlock,
    new GetBlockById,
    new GetBlockByHeight,
    new GetBlockCount,
    new ProduceBlock,
    new Transfer,
    new GetTransaction,
    new ImportPrivateKey,
    new HelpC,
    new QuitC,
    new ExitC
  ).groupBy(_.cmd)
}


