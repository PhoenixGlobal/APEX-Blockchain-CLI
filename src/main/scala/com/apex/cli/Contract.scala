package com.apex.cli

import java.nio.file.{Files, Paths}
import spray.json.JsNull
import com.apex.core.{Transaction, TransactionType}
import com.apex.crypto.{BinaryData, FixedNumber, UInt160}
import com.apex.solidity.Abi
import com.apex.solidity.compiler.{CompilationResult, SolidityCompiler}
import com.apex.solidity.compiler.SolidityCompiler.Options.{ABI, BIN, INTERFACE, METADATA}
import org.junit.Assert
import play.api.libs.json.{Json}
import scala.io.Source

/*
 * Copyright  2018 APEX Technologies.Co.Ltd. All rights reserved.
 *
 * FileName: Contract.scala
 *
 * @author: whitney.wei@chinapex.com: 19-01-10 @version: 1.0
 */
class ContractCommand extends CompositeCommand {

  override val cmd: String = "contract"
  override val description: String = "Operate smart contract"
  override val composite: Boolean = true

  override val subCommands: Seq[Command] = Seq(
    new CompileCommand,
    new DeployCommand,
    new CallCommand,
    new GetContractCommand
  )

  class CompileCommand extends Command {
      override val cmd = "compile"
    override val description = "Compile smart contract"

    override val paramList: ParameterList = ParameterList.create(
      new StringParameter("contractName", "n", "The contract name."),
    new StringParameter("source", "s", "Source code file name of smart contract.")
    )

    override def execute(params: List[String]): Result = {
      try {
        val checkResult = Account.checkWalletStatus
        if (!checkResult.isEmpty) InvalidParams(checkResult)
        else {
          val name = paramList.params(0).asInstanceOf[StringParameter].value
          val sourceFile = paramList.params(1).asInstanceOf[StringParameter].value
          // 获取需要编译的合约文件
          val compileContent = readFile(sourceFile)
          if (compileContent.isEmpty) InvalidParams("compile content is empty, please type a different one")
          else {

            val source = Paths.get(sourceFile)
            val res: SolidityCompiler.Result = SolidityCompiler.compile(source.toFile, true, Seq(ABI, BIN, INTERFACE, METADATA))
            val result = CompilationResult.parse(res.output)
            WalletCache.reActWallet

            if (result.getContract(name) != null) {
              Success("bin:"+result.getContract(name).bin+" \n abi："+result.getContract(name).abi)
            } else {
              Assert.fail()
              Success("false")
            }
          }
        }
      } catch {
        case e: Throwable => Error(e)
      }
  }}

  class DeployCommand extends Command {
    override val cmd = "deploy"
    override val description = "Deploy smart contract"

    override val paramList: ParameterList = ParameterList.create(
      new NicknameParameter("from", "from",
        "The account where the asset come from. Omit it if you want to send your tokens to the default account in the active wallet.", true),
      new StringParameter("data", "data", "Bin data file name of deploy smart contract.")
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

          val dataSource = paramList.params(1).asInstanceOf[StringParameter].value
          val dataContent = readFile(dataSource)

          if (!Account.checkAccountStatus(from)) InvalidParams("from account not exists, please type a different one")
          else if(dataContent.isEmpty) InvalidParams("data is empty, please type a different one")
          else {
            val tx = buildTx(TransactionType.Deploy, from, UInt160.Zero, BinaryData(dataContent))
            val result = sendTx(tx)

            WalletCache.reActWallet
            if(result.toString().toBoolean) Success("contractAdd:"+tx.getContractAddress().get)
            else Success("false")
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
      new NicknameParameter("from", "from", "The account where the asset come from. Omit it if you want to send your tokens to the default account in the active wallet.", true),
      new StringParameter("to", "to", "The target contract address."),
      new StringParameter("abi", "abi", "Path of the ABI file corresponding to the smart contract"),
      new StringParameter("method", "m", "The method in the smart contract")
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
          // 调用智能合约的方法名及参数
          val method = paramList.params(3).asInstanceOf[StringParameter].value

          // 根据abi获取文件内容
          val abiContent = readFile(abiFilePath)
          if (abiContent.isEmpty) InvalidParams("Abi content is empty, please type a different one")
          else if (!Account.checkAccountStatus(from)) InvalidParams("from account not exists, please type a different one")
          else {
            // 获取abi对象
            val abiJson = Abi.fromJson(abiContent)
            val data = Abi.fromJson(abiContent).encode(method)

            val tx = buildTx(TransactionType.Call, from, UInt160.fromBytes(BinaryData(to)), data)
            val txResult = sendTx(tx)

            WalletCache.reActWallet

            if(txResult.toString().toBoolean == true){

              Thread.sleep(1000)

              val result = RPC.post("getContract", s"""{"id":"${tx.id()}"}""")
              if(result.toString.equals("null")){
                Success("no result, txId is "+tx.id()+", type \"contract getContract\" to see contract status later.")
              }else Success(Json prettyPrint result)

            }else Success("false")
          }
        }
      } catch {
        case e: Throwable => Error(e)
      }
    }
  }


  class GetContractCommand
    extends Command {
    override val cmd = "get"
    override val description = "Get contract by transaction id"

    override val paramList: ParameterList = ParameterList.create(
      new StringParameter("id", "id", "The transaction id of contract."),
    )

    override def execute(params: List[String]): Result = {
      try {
        val checkResult = Account.checkWalletStatus
        if (!checkResult.isEmpty) InvalidParams(checkResult)
        else {
          val id = paramList.params(0).asInstanceOf[StringParameter].value
          val result = RPC.post("getContract", s"""{"id":"${id}"}""")
          if(result.toString.equals("null")){
            Success("no result.")
          }else Success(Json prettyPrint result)
        }
      } catch {
        case e: Throwable => Error(e)
      }
    }}


  def buildTx(txType:TransactionType.Value, from:String, to:UInt160, data:Array[Byte]) = {

    val privKey = Account.getAccount(from).getPrivKey()

    val account = RPC.post("showaccount", s"""{"address":"${privKey.publicKey.address}"}""")

    var nextNonce: Long = 0
    if (account != JsNull && account.toString() != "null") {
      nextNonce = (account \ "nextNonce").as[Long]
    }
    val tx = new Transaction(txType,
      privKey.publicKey.pubKeyHash,
      to,
      FixedNumber.Zero,
      nextNonce,
      data,
      FixedNumber.Zero,
      7000000,
      BinaryData.empty)
    tx.sign(privKey)

    tx
  }

  def sendTx(tx:Transaction) = {

    val txRawData = BinaryData(tx.toBytes)
    val rawTx: String = "{\"rawTx\":\"" + txRawData.toString + "\"}"
    val result = RPC.post("sendrawtransaction", rawTx)
    result
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