package com.apex.cli

import java.io.{File, FileWriter, PrintWriter}
import java.nio.file.{Files, Paths}
import com.apex.core.{Transaction, TransactionType}
import com.apex.crypto.{BinaryData, Ecdsa, FixedNumber, UInt160}
import com.apex.solidity.Abi
import com.apex.solidity.compiler.{CompilationResult, SolidityCompiler}
import com.apex.solidity.compiler.SolidityCompiler.Options.{ABI, BIN, INTERFACE, METADATA}
import org.junit.Assert

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
    new CallCommand
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
        WalletCache.reActWallet
        // 获取需要编译的合约文件
        val compileContent = AssetCommand.readFile(sourceFile)
        if (compileContent.isEmpty) InvalidParams("compile content is empty, please type a different one")
        else {

          val source = Paths.get(sourceFile)
          val res: SolidityCompiler.Result = SolidityCompiler.compile(source.toFile, true, Seq(ABI, BIN, INTERFACE, METADATA))
          val result = CompilationResult.parse(res.output)

          if (result.getContract(name) != null) {
            writeFile(writePath + "\\" + name + "bin.txt", result.getContract(name).bin)
            writeFile(writePath + "\\" + name + "abi.txt", result.getContract(name).abi)
            Success("The contract compile is successful.")
          } else {
            Assert.fail()
            Success("false")
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
      new GasPriceParameter("gasPrice", "gasPrice", "The price of gas that the transaction / contract is willing to pay.")
    )

    // 测试 data 6080604052348015600f57600080fd5b50603580601d6000396000f3006080604052600080fd00a165627a7a723058200b864e4f01cfb799a414a6ebdb9b63ce9225b82a293a346c33b42e691cdec0300029
    override def execute(params: List[String]): Result = {
      try {
        val checkResult = Account.checkWalletStatus
        if (!checkResult.isEmpty) InvalidParams(checkResult)
        else {
          WalletCache.reActWallet
          // 赋值from昵称
          var from = WalletCache.getActivityWallet().implyAccount
          // 根据昵称获取转账地址
          if (params.size / 2 == paramList.params.size) from = paramList.params(0).asInstanceOf[NicknameParameter].value

          val dataSource = paramList.params(1).asInstanceOf[StringParameter].value
          val dataContent = AssetCommand.readFile(dataSource)

          if (!Account.checkAccountStatus(from)) InvalidParams("from account not exists, please type a different one")
          else if (dataContent.isEmpty) InvalidParams("data is empty, please type a different one")
          else {
            val gasLimit = paramList.params(2).asInstanceOf[GasParameter].value
            val price = paramList.params(3).asInstanceOf[GasPriceParameter].value
            val gasPrice = AssetCommand.calcGasPrice(price)

            val tx = AssetCommand.buildTx(TransactionType.Deploy, from, UInt160.Zero, FixedNumber.Zero, BinaryData(dataContent), gasLimit = BigInt(gasLimit), gasPrice = gasPrice)
            val rpcTxResult = AssetCommand.sendTx(tx)

            if (!ChainCommand.checkSucceed(rpcTxResult)) {
              ChainCommand.returnFail(rpcTxResult)
            } else if (ChainCommand.getBooleanRes(rpcTxResult)) {
              Success("The contract broadcast is successful , the transaction hash is " + tx.id() + " , the contract address is " + tx.getContractAddress().get.address)
            } else Success("The contract broadcast failed. Please try again.")
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
      new GasPriceParameter("gasPrice", "gasPrice", "The price of gas that the transaction / contract is willing to pay.")
    )

    override def execute(params: List[String]): Result = {
      try {
        val checkResult = Account.checkWalletStatus
        if (!checkResult.isEmpty) InvalidParams(checkResult)
        else {
          WalletCache.reActWallet
          // 赋值from昵称
          var from = WalletCache.getActivityWallet().implyAccount
          // 根据昵称获取转账地址
          if (params.size / 2 == paramList.params.size) from = paramList.params(0).asInstanceOf[NicknameParameter].value

          // 合约地址
          val to = paramList.params(1).asInstanceOf[ContractAddressParameter].value
          // abi文件路径
          val abiFilePath = paramList.params(2).asInstanceOf[StringParameter].value
          // 调用智能合约的方法名及参数
          val method = paramList.params(3).asInstanceOf[StringParameter].value

          // 根据abi获取文件内容
          val abiContent = AssetCommand.readFile(abiFilePath)
          if (abiContent.isEmpty) InvalidParams("Abi content is empty, please type a different one")
          else if (!Account.checkAccountStatus(from)) InvalidParams("from account not exists, please type a different one")
          else {
            // 获取abi对象
            val data = Abi.fromJson(abiContent).encode(method)
            if (data.size / 8 < 4) InvalidParams("method not exists, please type a different one")

            val gasLimit = paramList.params(4).asInstanceOf[GasParameter].value
            val price = paramList.params(5).asInstanceOf[GasPriceParameter].value
            val gasPrice = AssetCommand.calcGasPrice(price)

            val tx = AssetCommand.buildTx(TransactionType.Call, from, Ecdsa.PublicKeyHash.fromAddress(to).get, FixedNumber.Zero, data, gasLimit = BigInt(gasLimit), gasPrice = gasPrice)
            val rpcTxResult = AssetCommand.sendTx(tx)

            if (ChainCommand.checkSucceed(rpcTxResult)) {

              Thread.sleep(1000)

              val rpcContractResult = RPC.post("getContract", s"""{"id":"${tx.id()}"}""")

              if (!ChainCommand.checkSucceed(rpcContractResult)) {
                ChainCommand.returnFail(rpcContractResult)
              } else if (ChainCommand.checkNotNull(rpcContractResult)) {
                ChainCommand.checkRes(rpcContractResult)
              } else Success("The contract broadcast is successful, type \"chain tx\" to see contract status later, the transaction hash is " + tx.id() + ".")

            } else ChainCommand.returnFail(rpcTxResult)
          }
        }
      } catch {
        case e: Throwable => Error(e)
      }
    }
  }

}