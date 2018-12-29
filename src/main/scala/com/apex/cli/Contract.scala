package com.apex.cli

import java.nio.file.{Files, Paths}

import spray.json.JsNull
import com.apex.core.{Transaction, TransactionType}
import com.apex.crypto.{BinaryData, Ecdsa, FixedNumber, UInt160, UInt256}
import com.apex.solidity.Abi
import play.api.libs.json.Json

import scala.io.Source

class ContractCommand extends CompositeCommand {

  override val cmd: String = "contract"
  override val description: String = "Operate smart contract"
  override val composite: Boolean = true

  override val subCommands: Seq[Command] = Seq(
    new CompileCommand,
    new DeployCommand,
    new CallCommand
  )

  class CompileCommand extends Command {
    override val cmd = "compile"
    override val description = "Compiler smart contract"

    override val paramList: ParameterList = ParameterList.create(
      new StringParameter("source", "s", "Source code file name of smart contract.")
    )

    override def execute(params: List[String]): Result = {
      try {
        val checkResult = Account.checkWalletStatus
        if (!checkResult.isEmpty) InvalidParams(checkResult)
        else {
          var source = paramList.params(0).asInstanceOf[NicknameParameter].value
          // 获取需要编译的合约文件
          val compileContent =  readFile(source)
          if (compileContent.isEmpty) InvalidParams("compile content is empty, please type a different one")
          else{

            val txRawData = BinaryData(compileContent)
            val rawTx: String = "{\"contract\":\""  + txRawData.toString  + "\"}"
            val result = RPC.post("compileContract",  rawTx)

            WalletCache.reActWallet
            Success(Json prettyPrint result)
          }
        }
      } catch {
        case e: Throwable => Error(e)
      }
    }
  }

  class DeployCommand extends Command {
    override val cmd = "deploy"
    override val description = "Deploy smart contract"

    override val paramList: ParameterList = ParameterList.create(
      new NicknameParameter("from", "from",
        "", true),
      new StringParameter("data", "data", "")
    )

    // 测试 data 6080604052348015600f57600080fd5b50603580601d6000396000f3006080604052600080fd00a165627a7a723058200b864e4f01cfb799a414a6ebdb9b63ce9225b82a293a346c33b42e691cdec0300029
    override def execute(params: List[String]): Result = {
      try {
        val checkResult = Account.checkWalletStatus
        if (!checkResult.isEmpty) InvalidParams(checkResult)
        else {
          // 赋值from昵称
          var from = WalletCache.getActivityWallet().implyAccount
          // 根据昵称获取转账地址
          if (params.size / 2 == paramList.params.size) from = paramList.params(0).asInstanceOf[NicknameParameter].value

          val data = paramList.params(1).asInstanceOf[StringParameter].value

          if (!Account.checkAccountStatus(from)) InvalidParams("from account not exists, please type a different one")
          else {
            val privKey = Account.getAccount(from).getPrivKey()

            val account = RPC.post("showaccount", s"""{"address":"${privKey.publicKey.address}"}""")

            var nextNonce: Long = 0
            if (account != JsNull) {
              nextNonce = (account \ "nextNonce").as[Long]
            }

            val tx = new Transaction(
              TransactionType.Deploy,
              privKey.publicKey,
              UInt160.Zero,
              "",
              FixedNumber.Zero,
              nextNonce,
              BinaryData(data),
              FixedNumber.Zero,
              Long.MaxValue,
              BinaryData.empty)
            tx.sign(privKey)

            val txRawData = BinaryData(tx.toBytes)
            val rawTx: String = "{\"rawTx\":\"" + txRawData.toString + "\"}"
            val result = RPC.post("sendrawtransaction", rawTx)
            WalletCache.reActWallet
            Success(Json prettyPrint result)
          }
        }
      } catch {
        case e: Throwable => Error(e)
      }
    }
  }

  // 测试合约地址
  class CallCommand extends Command {
    override val cmd = "call"
    override val description = "Call special function of smart contract"

    override val paramList: ParameterList = ParameterList.create(
      new NicknameParameter("from", "from",
        "", true),
      new StringParameter("to", "to",
        "Address of smart contract."),
      new StringParameter("abi", "abi", "Name of abi file"),
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

          // 合约地址
          val to = paramList.params(1).asInstanceOf[StringParameter].value
          // abi文件路径
          val abiFilePath = paramList.params(2).asInstanceOf[StringParameter].value

          // 根据abi获取文件内容
          val abiContent = readFile(abiFilePath)
          if (abiContent.isEmpty) InvalidParams("Abi content is empty, please type a different one")
          else if (!Account.checkAccountStatus(from)) InvalidParams("from account not exists, please type a different one")
          else {
            // 获取abi对象
            val abiJson = Abi.fromJson(abiContent)
            val privKey = Account.getAccount(from).getPrivKey()

            val account = RPC.post("showaccount", s"""{"address":"${privKey.publicKey.address}"}""")

            var nextNonce: Long = 0
            if (account != JsNull) {
              nextNonce = (account \ "nextNonce").as[Long]
            }

            val tx = new Transaction(TransactionType.Call,
              privKey.publicKey,
              UInt160.fromBytes(BinaryData(to)),
              "",
              FixedNumber.Zero,
              nextNonce,
              abiJson.encode(abiContent),
              FixedNumber.Zero,
              Long.MaxValue,
              BinaryData.empty)
            tx.sign(privKey)

            val txRawData = BinaryData(tx.toBytes)
            val rawTx: String = "{\"rawTx\":\"" + txRawData.toString + "\"}"
            val result = RPC.post("sendrawtransaction", rawTx)
            WalletCache.reActWallet
            Success(Json prettyPrint result)
          }
        }
      } catch {
        case e: Throwable => Error(e)
      }
    }
  }


  private def readFile(fileName: String): String = {
    var content = ""
    if (Files.exists(Paths.get(fileName))) {
      val file = Source.fromFile(fileName)
      content = file.getLines.mkString
      file.close()
    }
    content
  }
}