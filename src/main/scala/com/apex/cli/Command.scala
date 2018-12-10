/*
 * Copyright  2018 APEX Technologies.Co.Ltd. All rights reserved.
 *
 * FileName: Command.scala
 *
 * @author: ruixiao.xiao@chinapex.com: 18-8-10 下午1:55@version: 1.0
 */

package com.apex.cli

import com.apex.core.{Transaction, TransactionType}
import com.apex.crypto.Ecdsa.PrivateKey
import com.apex.crypto.{BinaryData, Crypto, Ecdsa, Fixed8, UInt256}
import play.api.libs.json.{JsNull, Json}

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

class GetBlocksCmd extends Command {
  override val cmd = "getblocks"
  override val description = "list block"
}

class GetBlockByIdCmd extends Command {
  override val cmd = "getblock"
  override val description = "get block by id"
  override val paramList: ParameterList = ParameterList.id
}

class GetBlockByHeightCmd extends Command {
  override val cmd = "getblock"
  override val description = "get block by height"
  override val paramList: ParameterList = ParameterList.int("height", "h")
}

class GetBlockCountCmd extends Command {
  override val cmd = "getblockcount"
  override val description = "get block count"
  override val paramList: ParameterList = ParameterList.empty
}

//class ProduceBlockCmd extends Command {
//  override val cmd = "produceblock"
//  override val description = "produce block"
//  override val paramList: ParameterList = ParameterList.empty
//}

class GetTransactionCmd extends Command {
  override val cmd = "gettx"
  override val description = "get transaction"
  override val paramList: ParameterList = ParameterList.id
}

class SendRawTransactionCmd extends Command {
  override val cmd = "sendrawtransaction"
  override val description = "send raw transaction"
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
        privKey.publicKey,
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

class SendCmd extends Command {
  override val cmd = "send"
  override val description = "transfer money"
  override val paramList: ParameterList = ParameterList.create(
    new AddressParameter("to", "to"),
    new AmountParameter()
  )

  override def execute(params: List[String]): Result = {
    try {
      val privKey = Wallet.Default.getPrivKey()

      val toAddress = paramList.params(0).asInstanceOf[AddressParameter].value
      val amount = paramList.params(1).asInstanceOf[AmountParameter].value

      val account = RPC.post("showaccount", s"""{"address":"${privKey.publicKey.address}"}""")

      var nextNonce: Long = 0
      if (account != JsNull) {
        nextNonce = (account \ "nextNonce").as[Long]
      }

      val tx = new Transaction(TransactionType.Transfer,
        privKey.publicKey,
        Ecdsa.PublicKeyHash.fromAddress(toAddress).get,
        "",
        Fixed8.fromDecimal(amount),
        UInt256.Zero,
        nextNonce,
        BinaryData.empty,
        BinaryData.empty)

      tx.sign(privKey)

      val txRawData = BinaryData(tx.toBytes)
      val rawTx: String = "{\"rawTx\":\""  + txRawData.toString  + "\"}"
      val result = RPC.post("sendrawtransaction", rawTx)

      Success(Json prettyPrint result)
    } catch {
      case e: Throwable => Error(e)
    }
  }
}

class ImportPrivateKeyCmd extends Command {
  override val cmd: String = "importprivkey"
  override val description: String = "import private key"
  override val paramList: ParameterList = ParameterList.str("key", "key")

  override def execute(params: List[String]): Result = {
    val key = params(1)

    if (Wallet.Default.importPrivKeyFromWIF(key)) {
      Wallet.save()
      Success("OK")
    }
    else {
      InvalidParams("key error")
    }
  }
}

class GetAccountCmd extends Command {
  override val cmd: String = "showaccount"
  override val description: String = "show account"
  override val paramList: ParameterList = ParameterList.address

}

class WalletInfoCmd extends Command {
  override val cmd = "walletinfo"
  override val description = "list wallet info"

  override def execute(params: List[String]): Result = {
    try {
      val privKey = Wallet.Default.getPrivKey()
      println(s"Address: ${privKey.publicKey.address}")
      Success("")
    } catch {
      case e: Throwable => Error(e)
    }
  }
}

class NewAddrCmd extends Command {
  override val cmd = "createaddr"
  override val description = "create new address"

  override def execute(params: List[String]): Result = {
    try {
      val privKey = new PrivateKey(BinaryData(Crypto.randomBytes(32)))
      println(s"Address: ${privKey.publicKey.address}")
      println(s"Private key (WIF): ${privKey.toWIF}")
      println(s"Private key (raw): ${privKey.toString}")
      println(s"Public key: ${privKey.publicKey.toString}")
      Success("")
    } catch {
      case e: Throwable => Error(e)
    }
  }
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
    new GetBlocksCmd,
    new GetBlockByIdCmd,
    new GetBlockByHeightCmd,
    new GetBlockCountCmd,
    //new ProduceBlockCmd,
    new SendRawTransactionCmd,
    new SendCmd,
    //new GetTransactionCmd,
    //new ImportPrivateKeyCmd,
    new GetAccountCmd,
    new WalletInfoCmd,
    new NewAddrCmd,
    new HelpC,
    new QuitC,
    new ExitC
  ).groupBy(_.cmd)
}


