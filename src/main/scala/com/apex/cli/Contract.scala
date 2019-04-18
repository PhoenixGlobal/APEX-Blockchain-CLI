package com.apex.cli

import java.io.{File, FileWriter, PrintWriter}
import java.nio.file.{Files, Paths}

import akka.actor.FSM.Failure
import akka.stream.FlowMonitorState.Failed
import com.apex.core.{Transaction, TransactionType}
import com.apex.crypto.{BinaryData, Ecdsa, FixedNumber, UInt160}
import com.apex.solidity.Abi
import com.apex.solidity.compiler.{CompilationResult, SolidityCompiler}
import com.apex.solidity.compiler.SolidityCompiler.Options.{ABI, BIN, INTERFACE, METADATA}
import javax.script.ScriptEngineManager
import jdk.nashorn.api.scripting.ScriptUtils
import org.junit.Assert

import scala.util.Try

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
    new RunCommand
  )

  class CompileCommand extends Command {
    override val cmd = "compile"
    override val description = "Compile smart contract"

    override val paramList: ParameterList = ParameterList.create(
      new StringParameter("contractName", "n", "The contract name to compile in the contract code."),
      new StringParameter("source", "s", "Path address of the contract source file."),
      new StringParameter("p", "p", "Output directories for BIN and ABI files.")
    )

    override def execute(params: List[String]): Result = {
      try {
        val name = paramList.params(0).asInstanceOf[StringParameter].value
        val sourceFile = paramList.params(1).asInstanceOf[StringParameter].value
        val writePath = paramList.params(2).asInstanceOf[StringParameter].value
        // 获取需要编译的合约文件
        val compileContent = Util.readFile(sourceFile)
        if (compileContent.isEmpty) InvalidParams("compile content is empty, please type a different one")
        else {

          val source = Paths.get(sourceFile)
          val res: SolidityCompiler.Result = SolidityCompiler.compile(source.toFile, true, Seq(ABI, BIN, INTERFACE, METADATA))
          if (res.errors.nonEmpty && res.output.isEmpty) {
            Success(res.errors)
          } else {
            val result = CompilationResult.parse(res.output)
            if (result.getContract(name) != null) {
              val bin = result.getContract(name).bin
              val abi = result.getContract(name).abi
              //              println("bin:" + bin)
              //              println("abi:" + abi)
              writeFile(writePath + "/" + name + "_bin.txt", bin)
              writeFile(writePath + "/" + name + "_abi.txt", abi)
              Success("The contract compile is successful.")
            } else {
              Assert.fail()
              Success("false, cannot get contract:" + name)
            }
          }
        }
      } catch {
        case e: Throwable => Error(e)
      }
    }

    def writeFile(filePath: String, data: String): Unit = {
      val file = new File(filePath)
      val fw = new FileWriter(file)
      fw.write(data)
      fw.close()
    }
  }

  class DeployCommand extends Command {
    override val cmd = "deploy"
    override val description = "Deploy smart contract"

    override val paramList: ParameterList = ParameterList.create(
      new NicknameParameter("from", "from",
        "The account where the asset come from. Omit it if you want to send your tokens to the default account in the active wallet.", true),
      new StringParameter("bin", "bin", "The BIN files corresponding this contract."),
      new GasParameter("gasLimit", "gasLimit", "Maximum number of gas this transactions/contract is willing to pay."),
      new GasPriceParameter("gasPrice", "gasPrice", "The price of gas that the transaction / contract is willing to pay."),
      new AmountParameter("amount", "amount", "The amount of the asset to be transfer.", true)
    )

    // 测试 data 6080604052348015600f57600080fd5b50603580601d6000396000f3006080604052600080fd00a165627a7a723058200b864e4f01cfb799a414a6ebdb9b63ce9225b82a293a346c33b42e691cdec0300029
    override def execute(params: List[String]): Result = {
      try {
        val checkResult = Account.checkWalletStatus
        if (!checkResult.isEmpty) InvalidParams(checkResult)
        else {
          WalletCache.reActWallet
          // 根据昵称获取转账地址
          var from = paramList.params(0).asInstanceOf[NicknameParameter].value
          // 取当前活跃帐号
          if (from == null) from = WalletCache.getActivityWallet().implyAccount

          val dataSource = paramList.params(1).asInstanceOf[StringParameter].value
          val dataContent = Util.readFile(dataSource)

          if (!Account.checkAccountStatus(from)) InvalidParams("from account not exists, please type a different one")
          else if (dataContent.isEmpty) InvalidParams("data is empty, please type a different one")
          else {
            val gasLimit = paramList.params(2).asInstanceOf[GasParameter].value
            val price = paramList.params(3).asInstanceOf[GasPriceParameter].value
            val gasPrice = Util.calcGasPrice(price)
            var amount = paramList.params(4).asInstanceOf[AmountParameter].value
            if (amount == null) amount = BigDecimal.apply(0.0)

            val privKey = Account.getAccount(from).getPrivKey()
            val account = RPC.post("showaccount", s"""{"address":"${privKey.publicKey.address}"}""")

            var nextNonce = Account.getResultNonce(account)
            val balance = Account.getResultBalance(account)
            if (BigDecimal.apply(balance) < amount) {
              InvalidParams("insufficient account balance")
            }
            else {
              val tx = Util.buildTx(TransactionType.Deploy, from, UInt160.Zero, FixedNumber.fromDecimal(amount), BinaryData(dataContent),
                true, nextNonce, gasLimit = BigInt(gasLimit), gasPrice = gasPrice)
              val rpcTxResult = Util.sendTx(tx)
              println("transaction hash is " + tx.id())

              if (!ChainCommand.checkTxSucceed(rpcTxResult)) {
                ChainCommand.returnTxFail(rpcTxResult)
              } else if (ChainCommand.getTxBooleanRes(rpcTxResult)) {
                Success("The contract broadcast is successful, the contract address is " + tx.getContractAddress().get.address)
              } else Success("The contract broadcast failed. Please try again.")
            }
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
    override val description = "Call smart contract"

    override val paramList: ParameterList = ParameterList.create(
      new NicknameParameter("from", "from", "The account where the asset come from. Omit it if you want to send your tokens to the default account in the active wallet.", true),
      new ContractAddressParameter("to", "to", "The contract address."),
      new StringParameter("abi", "abi", "The ABI files corresponding this contract."),
      new StringParameter("method", "m", "The method name of called contract."),
      new GasParameter("gasLimit", "gasLimit", "Maximum number of gas this transactions/contract is willing to pay."),
      new GasPriceParameter("gasPrice", "gasPrice", "The price of gas that the transaction / contract is willing to pay."),
      new AmountParameter("amount", "amount", "The amount of the asset to be transfer.", true)
    )

    override def execute(params: List[String]): Result = {
      try {
        val checkResult = Account.checkWalletStatus
        if (!checkResult.isEmpty) InvalidParams(checkResult)
        else {
          WalletCache.reActWallet
          // 根据昵称获取转账地址
          var from = paramList.params(0).asInstanceOf[NicknameParameter].value
          // 取当前活跃帐号
          if (from == null) from = WalletCache.getActivityWallet().implyAccount

          // 合约地址
          val to = paramList.params(1).asInstanceOf[ContractAddressParameter].value
          // abi文件路径
          val abiFilePath = paramList.params(2).asInstanceOf[StringParameter].value
          // 调用智能合约的方法名及参数
          val method = paramList.params(3).asInstanceOf[StringParameter].value

          // 根据abi获取文件内容
          val abiContent = Util.readFile(abiFilePath)
          if (abiContent.isEmpty) InvalidParams("Abi content is empty, please type a different one")
          else if (!Account.checkAccountStatus(from)) InvalidParams("from account not exists, please type a different one")
          else {
            // 获取abi对象
            val data = Abi.fromJson(abiContent).encode(method)
            if (data.size / 8 < 4) InvalidParams("method not exists, please type a different one")

            val gasLimit = paramList.params(4).asInstanceOf[GasParameter].value
            val price = paramList.params(5).asInstanceOf[GasPriceParameter].value
            val gasPrice = Util.calcGasPrice(price)

            var amount = paramList.params(6).asInstanceOf[AmountParameter].value
            if (amount == null) amount = BigDecimal.apply(0.0)

            val privKey = Account.getAccount(from).getPrivKey()
            val account = RPC.post("showaccount", s"""{"address":"${privKey.publicKey.address}"}""")

            var nextNonce = Account.getResultNonce(account)
            val balance = Account.getResultBalance(account)
            if (BigDecimal.apply(balance) < amount) {
              InvalidParams("insufficient account balance")
            }
            else {
              val tx = Util.buildTx(TransactionType.Call, from, Ecdsa.PublicKeyHash.fromAddress(to).get, FixedNumber.fromDecimal(amount), data,
                true, nextNonce, gasLimit = BigInt(gasLimit), gasPrice = gasPrice)
              val rpcTxResult = Util.sendTx(tx)

              if (ChainCommand.checkTxSucceed(rpcTxResult)) {
                Thread.sleep(1000)
                val rpcContractResult = RPC.post("getContract", s"""{"id":"${tx.id()}"}""")
                if (!ChainCommand.checkSucceed(rpcContractResult)) {
                  ChainCommand.returnFail(rpcContractResult)
                } else if (ChainCommand.checkNotNull(rpcContractResult)) {
                  ChainCommand.checkRes(rpcContractResult)
                } else Success("The contract broadcast is successful, type \"chain tx\" to see contract status later, the transaction hash is " + tx.id() + ".")

              } else ChainCommand.returnTxFail(rpcTxResult)
            }
          }
        }
      } catch {
        case e: Throwable => Error(e)
      }
    }
  }

}

case class DeployInfo(gasLimit: BigInt, gasPrice: FixedNumber, amount: FixedNumber, contractPath: String, contractName: String, args: Array[Object])

case class CallInfo(gasLimit: Long, gasPrice: Long, amount: Long, contractPath: String, contractName: String, args: Array[AnyRef] = Array.empty)

object RunCommand {
  val script =
    "var BigInt = Java.type('scala.math.BigInt');\n" +
      "var FixedNumber = Java.type('com.apex.crypto.FixedNumber');\n" +
      "var DeployInfo = Java.type('com.apex.cli.DeployInfo');\n" +
      "var CallInfo = Java.type('com.apex.cli.CallInfo');\n" +
      "function deploy(gas,price,amount,path,name) {\n" +
      "   var gasLimit = BigInt.apply(gas);\n" +
      "   var gasPrice = new FixedNumber(BigInt.apply(price));\n" +
      "   var value = new FixedNumber(BigInt.apply(amount));" +
      "   var args = Java.to(Array.prototype.slice.call(arguments,5), 'java.lang.Object[]');" +
      "   var deploy = new DeployInfo(gasLimit,gasPrice,value,path,name,args)\n" +
      "   return deploy;\n" +
      "}\n" +
      "function call(gasLimit,gasPrice,amount,contractPath,contractName) {\n" +
      "    return new DeployInfo(gasLimit,gasPrice,amount,contractPath,contractName)\n" +
      "}\n"
}

class RunCommand extends Command {

  import RunCommand._

  override val cmd: String = "run"
  override val description: String = "deploy or call contract. eg. deploy(gasLimit, gasPrice, amount, contractPath, contractName, contractArgs...) or call(gasLimit, gasPrice, amount, contractAddress, abiPath, functionName, args...)"
  override val paramList: ParameterList = ParameterList.create(new StringParameter("p", "p", "deploy(gasLimit)\ncall(gasLimit)"))

  override def execute(params: List[String]): Result = {
    try {
      val manager = new ScriptEngineManager
      val engine = manager.getEngineByName("nashorn")
      val input = paramList.params(0).asInstanceOf[StringParameter].value
      Try(engine.eval(s"$script$input")) match {
        case util.Success(result) => result match {
          case info: DeployInfo => deploy(info)
          case info: CallInfo => call(info)
          case _ => InvalidParams(input)
        }
        case util.Failure(e) => {
          println(e.getMessage)
          InvalidParams(input)
        }
      }
    } catch {
      case e: Throwable => Error(e)
    }
  }

  private def deploy(info: DeployInfo): Result = {
    val wallet = WalletCache.getActivityWallet
    if (wallet == null) {
      //      throw new Exception("no active wallet")
    }

    val from = "" //wallet.implyAccount
    val source = Paths.get(info.contractPath)
    val res: SolidityCompiler.Result = SolidityCompiler.compile(source.toFile, true, Seq(ABI, BIN, INTERFACE, METADATA))
    val result = CompilationResult.parse(res.output)
    val contract = result.getContract(info.contractName)
    val abi = Abi.fromJson(contract.abi)
    val ctorData = abi.findConstructor match {
      case Some(ctor) => {
        if (ctor.inputs.length != info.args.length) {
          val expect = ctor.inputs.map(_.solidityType.toString).mkString(",")
          val actual = info.args.map(_.getClass.getName).mkString(",")
          throw new Exception(s"constructor not match. expect constructor($expect) actual constructor($actual)")
        }
        ctor.encodeArguments(info.args)
      }
      case None => Array.empty
    }
    val data = BinaryData(contract.bin).toArray ++ ctorData
    val privKey = Account.getAccount(from).getPrivKey()
    val account = RPC.post("showaccount", s"""{"address":"${privKey.publicKey.address}"}""")
    val nextNonce = Account.getResultNonce(account)
    val tx = Util.buildTx(TransactionType.Deploy, from, UInt160.Zero, info.amount, data,
      true, nextNonce, info.gasPrice, info.gasLimit)
    val rpcTxResult = Util.sendTx(tx)

    if (!ChainCommand.checkTxSucceed(rpcTxResult)) {
      ChainCommand.returnTxFail(rpcTxResult)
    } else if (ChainCommand.getTxBooleanRes(rpcTxResult)) {
      Success("The contract broadcast is successful , the transaction hash is " + tx.id() + " , the contract address is " + tx.getContractAddress().get.address)
    } else Success("The contract broadcast failed. Please try again.")
  }

  private def call(info: CallInfo): Result = {
    throw new NotImplementedError
  }
}