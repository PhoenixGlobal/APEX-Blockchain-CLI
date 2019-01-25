package com.apex.cli

import com.apex.consensus.{RegisterData, VoteData, WitnessInfo}
import com.apex.core.{OperationType, Transaction, TransactionType}
import com.apex.crypto.{BinaryData, FixedNumber, UInt160}
import com.apex.vm.DataWord
import play.api.libs.json.Json
import spray.json.JsNull

/*
 * Copyright  2018 APEX Technologies.Co.Ltd. All rights reserved.
 *
 * FileName: .scala
 *
 * @author: whitney.wei@chinapex.com: 19-01-22 @version: 1.0
 */
class ProduceCommand extends CompositeCommand  {
  override val cmd: String = "produce"
  override val description: String = "Operate of produce"
  override val composite: Boolean = true

  private val registerNodeAddr = DataWord.of("0000000000000000000000000000000000000000000000000000000000000009")
  private val voteAddr = DataWord.of("0000000000000000000000000000000000000000000000000000000000000010")

  override val subCommands: Seq[Command] = Seq(
    new RegisterCommand,
    new ResisterCancelCommand,
    new VoteCommand,
    new VoteCancelCommand
  )

  def buildTx(txType:TransactionType.Value, from:String, to:UInt160, data:Array[Byte]) = {

    val privKey = Account.getAccount(from).getPrivKey()

    val account = RPC.post("showaccount", s"""{"address":"${privKey.publicKey.address}"}""")

    var nextNonce: Long = 0
    if (account != JsNull) {
      nextNonce = (account \ "nextNonce").as[Long]
    }
    val tx = new Transaction(txType,
      privKey.publicKey.pubKeyHash,
      to,
      "",
      FixedNumber.Zero,
      nextNonce,
      data,
      FixedNumber.Zero,
      Long.MaxValue,
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


  class RegisterCommand extends Command {
    override val cmd = "reg"
    override val description = "Register produce"

    override val paramList: ParameterList = ParameterList.create(
      new NicknameParameter("from", "from", "", true),
      new StringParameter("url", "url", ""),
      new StringParameter("country", "country", ""),
      new StringParameter("address", "address", ""),
      new IntParameter("longitude", "longitude", ""),
      new IntParameter("latitude", "latitude", "")
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

          if (!Account.checkAccountStatus(from)) InvalidParams("from account not exists, please type a different one")
          else {
            val fromHash = Account.getAccount(from).getPrivKey().publicKey.pubKeyHash
            val url = paramList.params(1).asInstanceOf[StringParameter].value
            val country = paramList.params(2).asInstanceOf[StringParameter].value
            val address = paramList.params(3).asInstanceOf[StringParameter].value
            val longitude = paramList.params(4).asInstanceOf[IntParameter].value
            val latitude = paramList.params(5).asInstanceOf[IntParameter].value

            val witnessInfo = new WitnessInfo(from, fromHash, url, country, address, longitude, latitude);
            val registerData = new RegisterData(fromHash, witnessInfo, OperationType.register)

            val tx = buildTx(TransactionType.Call, from, UInt160.fromBytes(registerNodeAddr.data), registerData.toBytes)
            val txResult = sendTx(tx)
            Success(Json prettyPrint txResult)
          }

        }
      } catch {
        case e: Throwable => Error(e)
      }
    }}

  class ResisterCancelCommand extends Command {
    override val cmd = "cancelReg"
    override val description = "Cancel register produce"

    override val paramList: ParameterList = ParameterList.create(
      new NicknameParameter("from", "from", "", true)
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

          if (!Account.checkAccountStatus(from)) InvalidParams("from account not exists, please type a different one")
          else {
            val fromHash = Account.getAccount(from).getPrivKey().publicKey.pubKeyHash

            val witnessInfo = new WitnessInfo(from, fromHash);
            val registerData = new RegisterData(fromHash, witnessInfo, OperationType.resisterCancel)

            val tx = buildTx(TransactionType.Call, from, UInt160.fromBytes(registerNodeAddr.data), registerData.toBytes)
            val txResult = sendTx(tx)
            Success(Json prettyPrint txResult)
          }

        }
      } catch {
        case e: Throwable => Error(e)
      }
    }}

  class VoteCommand extends Command {
    override val cmd = "vote"
    override val description = "Vote for produce"

    override val paramList: ParameterList = ParameterList.create(
      new NicknameParameter("from", "from", "", true),
      new StringParameter("candidate", "candidate", ""),
      new AmountParameter("count", "count", "")
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

          if (!Account.checkAccountStatus(from)) InvalidParams("from account not exists, please type a different one")
          else {
            val fromHash = Account.getAccount(from).getPrivKey().publicKey.pubKeyHash
            val candidate = paramList.params(1).asInstanceOf[StringParameter].value
            val count = paramList.params(2).asInstanceOf[AmountParameter].value

            val voteData = new VoteData(UInt160.fromBytes(candidate.getBytes), FixedNumber.fromDecimal(count), OperationType.register)

            val tx = buildTx(TransactionType.Call, from, UInt160.fromBytes(voteAddr.data), voteData.toBytes)
            val txResult = sendTx(tx)
            Success(Json prettyPrint txResult)
          }
        }
      } catch {
        case e: Throwable => Error(e)
      }
    }
  }

  class VoteCancelCommand extends Command {
    override val cmd = "cancelVote"
    override val description = "Cancel vote"

    override val paramList: ParameterList = ParameterList.create(
      new NicknameParameter("from", "from", "", true),
      new StringParameter("candidate", "candidate", ""),
      new AmountParameter("count", "count", "")
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

          if (!Account.checkAccountStatus(from)) InvalidParams("from account not exists, please type a different one")
          else {
            val fromHash = Account.getAccount(from).getPrivKey().publicKey.pubKeyHash
            val candidate = paramList.params(1).asInstanceOf[StringParameter].value
            val count = paramList.params(2).asInstanceOf[AmountParameter].value

            val voteData = new VoteData(UInt160.fromBytes(candidate.getBytes), FixedNumber.fromDecimal(count), OperationType.register)

            val tx = buildTx(TransactionType.Call, from, UInt160.fromBytes(voteAddr.data), voteData.toBytes)
            val txResult = sendTx(tx)
            Success(Json prettyPrint txResult)
          }
        }
      } catch {
        case e: Throwable => Error(e)
      }
    }
  }

  class GetWitnessCommand extends Command {
    override val cmd = "getWitness"
    override val description = "List of all witness"

    override def execute(params: List[String]): Result = {
      try {
        val result = RPC.post("getWitness", "")
        Success(Json prettyPrint result)
      } catch {
        case e: Throwable => Error(e)
      }
    }
  }

  class GetVoteByAddrCommand extends Command {
    override val cmd = "getVoteByAddr"
    override val description = "Get Vote By Address"

    override val paramList: ParameterList = ParameterList.create(
      new StringParameter("address", "address", "")
    )

    override def execute(params: List[String]): Result = {
      try {
        val address = paramList.params(0).asInstanceOf[StringParameter].value
        val result = RPC.post("getVoteByAddr", s"""{"address":"${address}"}""")
        Success(Json prettyPrint result)
      } catch {
        case e: Throwable => Error(e)
      }
    }
  }

  class GetProducesCommand extends Command {
    override val cmd = "getProducers"
    override val description = "list of producer"

    override val paramList: ParameterList = ParameterList.create(
      new StringParameter("type", "type", "")
    )

    override def execute(params: List[String]): Result = {
      try {
        val listType = paramList.params(0).asInstanceOf[StringParameter].value
        val result = RPC.post("getProducers", s"""{"listType":"${listType}"}""")
        Success(Json prettyPrint result)
      } catch {
        case e: Throwable => Error(e)
      }
    }
  }

}


