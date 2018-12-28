package com.apex.cli

import java.nio.file.{Files, Paths}

import spray.json.JsNull
import com.apex.core.{Transaction, TransactionType}
import com.apex.crypto.{BinaryData, Ecdsa, Fixed8, UInt160, UInt256}
import com.apex.solidity.Abi
import com.apex.vm.DataWord
import play.api.libs.json.Json

import scala.io.Source

class ContractCommand extends CompositeCommand {

  override val cmd: String = "contract"
  override val description: String = "Operate smart contract"
  override val composite: Boolean = true

  override val subCommands: Seq[Command] = Seq(
    new DeployCommand,
    new CallCommand
  )
}

class DeployCommand extends Command {
  override val cmd = "deploy"
  override val description = "Deploy smart contract"

  override val paramList: ParameterList = ParameterList.create(
    new NicknameParameter("from", "from",
      "",true),
    new StringParameter("data", "data","")
  )

  // 测试 data 608060405234801561001057600080fd5b5060e68061001f6000396000f3fe6080604052600436106043576000357c01000000000000000000000000000000000000000000000000000000009004806360fe47b11460485780636d4ce63c14607f575b600080fd5b348015605357600080fd5b50607d60048036036020811015606857600080fd5b810190808035906020019092919050505060a7565b005b348015608a57600080fd5b50609160b1565b6040518082815260200191505060405180910390f35b8060008190555050565b6000805490509056fea165627a7a723058202c7cfe05b5e1b84938fa70727102e914fba062d91fde5a0f0a92613ad081732b0029
  override def execute(params: List[String]): Result = {
    try{
      val checkResult = Account.checkWalletStatus
      if(!checkResult.isEmpty) InvalidParams(checkResult)
      else {
        // 赋值from昵称
        var from = WalletCache.getActivityWallet().implyAccount
        // 根据昵称获取转账地址
        if(params.size/2 == paramList.params.size)  from = paramList.params(0).asInstanceOf[NicknameParameter].value

        val data = paramList.params(1).asInstanceOf[StringParameter].value

        if(!Account.checkAccountStatus(from)) InvalidParams("from account not exists, please type a different one")
        else{
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
            Fixed8.Zero,
            UInt256.Zero,
            nextNonce,
            BinaryData(data),
            BinaryData.empty,
            DataWord.of(DataWord.MAX_VALUE.byteValue()).data,
            BinaryData.empty)
          tx.sign(privKey)

          val txRawData = BinaryData(tx.toBytes)
          val rawTx: String = "{\"rawTx\":\""  + txRawData.toString  + "\"}"
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
      "",true),
    new StringParameter("to", "to",
      "Address of smart contract."),
    new StringParameter("abi", "abi","Name of abi file"),
  )

  override def execute(params: List[String]): Result = {
    try{
      val checkResult = Account.checkWalletStatus
      if(!checkResult.isEmpty) InvalidParams(checkResult)
      else {
        // 赋值from昵称
        var from = WalletCache.getActivityWallet().implyAccount
        // 根据昵称获取转账地址
        if(params.size/2 == paramList.params.size)  from = paramList.params(0).asInstanceOf[NicknameParameter].value

        // 合约地址
        val to = paramList.params(1).asInstanceOf[StringParameter].value
        // abi文件路径
        val abiFilePath = paramList.params(2).asInstanceOf[StringParameter].value

        // 根据abi获取文件内容
        val abiContent  = readAbiFile(abiFilePath)
        // 获取abi对象
        val abiJson = Abi.fromJson(abiContent)

        if(!Account.checkAccountStatus(from)) InvalidParams("from account not exists, please type a different one")
        else{
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
            Fixed8.Zero,
            UInt256.Zero,
            nextNonce,
            abiJson.encode(abiContent),
            BinaryData.empty,
            DataWord.of(DataWord.MAX_VALUE.byteValue()).data,
            BinaryData.empty)
          tx.sign(privKey)

          val txRawData = BinaryData(tx.toBytes)
          val rawTx: String = "{\"rawTx\":\""  + txRawData.toString  + "\"}"
          val result = RPC.post("sendrawtransaction", rawTx)
          WalletCache.reActWallet
          Success(Json prettyPrint result)
        }
      }
    } catch {
      case e: Throwable => Error(e)
    }
  }

  private def readAbiFile(abiFileName : String): String ={
    var abiContent = ""
    if(Files.exists(Paths.get(abiFileName))){
      val file = Source.fromFile(abiFileName)
      abiContent = file.getLines.mkString
      file.close()
    }
    abiContent
  }
}

