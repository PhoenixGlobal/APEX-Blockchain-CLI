package com.apex.cli

import com.apex.consensus.{RegisterData, VoteData, WitnessInfo}
import com.apex.core.{OperationType, Transaction, TransactionType}
import com.apex.crypto.Ecdsa.PublicKeyHash
import com.apex.crypto.{BinaryData, FixedNumber, UInt160, UInt256}
import com.apex.vm.DataWord
import play.api.libs.json.{JsNull, JsValue, Json}

/*
 * Copyright  2018 APEX Technologies.Co.Ltd. All rights reserved.
 *
 * FileName: .scala
 *
 * @author: whitney.wei@chinapex.com: 19-01-22 @version: 1.0
 */
class ProducerCommand extends CompositeCommand {
  override val cmd: String = "producer"
  override val description: String = "Operate producer"
  override val composite: Boolean = true

  val registerNodeAddr = DataWord.of("0000000000000000000000000000000000000000000000000000000000000101")
  val voteAddr = DataWord.of("0000000000000000000000000000000000000000000000000000000000000102")

  override val subCommands: Seq[Command] = Seq(
    new RegisterCommand,
    new ResisterCancelCommand,
    new VoteCommand,
    new VoteCancelCommand,
    new GetByAddrCommand,
    new ListCommand,
    new GetVoteCommand
  )

  class RegisterCommand extends Command {
    override val cmd = "reg"
    override val description = "Register as an spare production node"

    override val paramList: ParameterList = ParameterList.create(
      new NicknameParameter("from", "from", "The account where the asset come from. Omit it if you want to send your tokens to the default account in the active wallet.", true),
      new StringParameter("url", "url", "The offical website."),
      new StringParameter("company", "company", "The name of producer node."),
      new StringParameter("country", "country", "The country where the production node is located."),
      new StringParameter("address", "address", "The contact address of the producer node,which is used to determind the order of all producers."),
      new IntParameter("longitude", "longitude", "The position of the production node in the whole network is used to determine the working order of all producers."),
      new IntParameter("latitude", "latitude", "The position of the production node in the whole network is used to determine the working order of all producers."),
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

          if (!Account.checkAccountStatus(from)) InvalidParams("from account not exists, please type a different one")
          else {
            val fromHash = Account.getAccount(from).getPrivKey().publicKey.pubKeyHash
            val url = paramList.params(1).asInstanceOf[StringParameter].value
            val company = paramList.params(2).asInstanceOf[StringParameter].value
            val country = paramList.params(3).asInstanceOf[StringParameter].value
            val address = paramList.params(4).asInstanceOf[StringParameter].value
            val longitude = paramList.params(5).asInstanceOf[IntParameter].value
            val latitude = paramList.params(6).asInstanceOf[IntParameter].value
            val price = paramList.params(7).asInstanceOf[GasPriceParameter].value
            val gasPrice = AssetCommand.calcGasPrice(price)

            val witnessInfo = new WitnessInfo(name = company, addr = fromHash, url = url, country = country, address = address, longitude = longitude, latitude = latitude)
            val registerData = new RegisterData(fromHash, witnessInfo, OperationType.register)
            val tx = AssetCommand.buildTx(TransactionType.Call, from, registerNodeAddr.toUInt160, FixedNumber.Zero, registerData.toBytes, gasPrice = gasPrice)
            val rpcTxResult = AssetCommand.sendTx(tx)

            printRes(rpcTxResult, tx.id())
          }

        }
      } catch {
        case e: Throwable => Error(e)
      }
    }
  }

  class ResisterCancelCommand extends Command {
    override val cmd = "unReg"
    override val description = "Unregister as a producer"

    override val paramList: ParameterList = ParameterList.create(
      new NicknameParameter("from", "from", "The account where the asset come from. Omit it if you want to send your tokens to the default account in the active wallet.",
        true),
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

          if (!Account.checkAccountStatus(from)) InvalidParams("from account not exists, please type a different one")
          else {
            val price = paramList.params(1).asInstanceOf[GasPriceParameter].value
            val gasPrice = AssetCommand.calcGasPrice(price)

            val fromHash = Account.getAccount(from).getPrivKey().publicKey.pubKeyHash

            val witnessInfo = new WitnessInfo(name = from, addr = fromHash)
            val registerData = new RegisterData(fromHash, witnessInfo, OperationType.resisterCancel)

            val tx = AssetCommand.buildTx(TransactionType.Call, from, registerNodeAddr.toUInt160, FixedNumber.Zero, registerData.toBytes, gasPrice = gasPrice)
            val rpcTxResult = AssetCommand.sendTx(tx)

            printRes(rpcTxResult, tx.id())
          }

        }
      } catch {
        case e: Throwable => Error(e)
      }
    }
  }

  class VoteCommand extends Command {
    override val cmd = "vote"
    override val description = "Vote for supported nodes"

    override val paramList: ParameterList = ParameterList.create(
      new NicknameParameter("from", "from", "The account where the asset come from. Omit it if you want to send your tokens to the default account in the active wallet.", true),
      new AddressParameter("address", "address", "The supported node address."),
      new AmountParameter("count", "count", "The number of votes."),
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

          if (!Account.checkAccountStatus(from)) InvalidParams("from account not exists, please type a different one")
          else {
            val candidate = paramList.params(1).asInstanceOf[AddressParameter].value
            val count = paramList.params(2).asInstanceOf[AmountParameter].value
            val price = paramList.params(3).asInstanceOf[GasPriceParameter].value
            val gasPrice = AssetCommand.calcGasPrice(price)

            val voteData = new VoteData(PublicKeyHash.fromAddress(candidate).get, FixedNumber.fromDecimal(count), OperationType.register)

            val tx = AssetCommand.buildTx(TransactionType.Call, from, voteAddr.toUInt160, FixedNumber.Zero, voteData.toBytes, gasPrice = gasPrice)
            val rpcTxResult = AssetCommand.sendTx(tx)
            printRes(rpcTxResult, tx.id())
          }
        }
      } catch {
        case e: Throwable => Error(e)
      }
    }
  }

  class VoteCancelCommand extends Command {
    override val cmd = "unVote"
    override val description = "Cancel voting on the node"

    override val paramList: ParameterList = ParameterList.create(
      new NicknameParameter("from", "from", "The account where the asset come from. Omit it if you want to send your tokens to the default account in the active wallet.", true),
      new AddressParameter("address", "address", "The supported node address."),
      new AmountParameter("count", "count", "The number of votes."),
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

          if (!Account.checkAccountStatus(from)) InvalidParams("from account not exists, please type a different one")
          else {
            val candidate = paramList.params(1).asInstanceOf[AddressParameter].value
            val count = paramList.params(2).asInstanceOf[AmountParameter].value
            val price = paramList.params(3).asInstanceOf[GasPriceParameter].value
            val gasPrice = AssetCommand.calcGasPrice(price)

            val voteData = new VoteData(PublicKeyHash.fromAddress(candidate).get, FixedNumber.fromDecimal(count), OperationType.resisterCancel)

            val tx = AssetCommand.buildTx(TransactionType.Call, from, voteAddr.toUInt160, FixedNumber.Zero, voteData.toBytes, gasPrice = gasPrice)
            val rpcTxResult = AssetCommand.sendTx(tx)
            printRes(rpcTxResult, tx.id())
          }
        }
      } catch {
        case e: Throwable => Error(e)
      }
    }
  }

  class ListCommand extends Command {
    override val cmd = "list"
    override val description = "Query node information"

    override val paramList: ParameterList = ParameterList.create(
      new StringParameter("type", "type", "Supports four query types,all:query all producer information;Previous:query the last round of production nodes.activate;Query the working production node;pending:Query the next round of production nodes with production rights. ")
    )

    override def execute(params: List[String]): Result = {
      try {
        val listType = paramList.params(0).asInstanceOf[StringParameter].value

        if (listType != "all" || listType != "active" || listType != "pending" || listType != "previous") {
          InvalidParams("type not exists, please type a different one")
        }

        val result = RPC.post("getProducers", s"""{"listType":"${listType}"}""")
        ChainCommand.checkRes(result)
      } catch {
        case e: Throwable => Error(e)
      }
    }
  }

  class GetByAddrCommand extends Command {
    override val cmd = "getByAddr"
    override val description = "Query node information by node address"

    override val paramList: ParameterList = ParameterList.create(
      new StringParameter("address", "address", "The address of the producer node.")
    )

    override def execute(params: List[String]): Result = {
      try {
        val address = paramList.params(0).asInstanceOf[StringParameter].value
        val rpcResult = RPC.post("getProducer", s"""{"address":"${address}"}""")

        if (!ChainCommand.checkSucceed(rpcResult)) {
          ChainCommand.returnFail(rpcResult)
        } else if (ChainCommand.checkNotNull(rpcResult)) {
          ChainCommand.returnSuccess(rpcResult)
        } else Success("No node information was found for this address.")

      } catch {
        case e: Throwable => Error(e)
      }
    }
  }

  class GetVoteCommand extends Command {
    override val cmd = "getVote"
    override val description = "Query this address for currently redeemable votes."

    override val paramList: ParameterList = ParameterList.create(
      new StringParameter("address", "a", "Query this address for currently redeemable votes.")
    )

    override def execute(params: List[String]): Result = {
      try {
        WalletCache.reActWallet
        val a = paramList.params(0).asInstanceOf[StringParameter].value

        var address = ""
        if (a.length == 35) address = a
        else if (Account.checkAccountStatus(a)) address = Account.getAccount(a).address

        if (address.isEmpty) InvalidParams("to account not exists, please type a different one")

        val rpcResult = RPC.post("getVote", s"""{"address":"${address}"}""")

        if (!ChainCommand.checkSucceed(rpcResult)) {
          ChainCommand.returnFail(rpcResult)
        } else if (ChainCommand.checkNotNull(rpcResult)) {
          ChainCommand.returnSuccess(rpcResult)
        } else Success("No redeemable votes were found..")

      } catch {
        case e: Throwable => Error(e)
      }
    }
  }

  def printRes(rpcTxResult: JsValue, hash: UInt256): Result = {

    if (!ChainCommand.checkTxSucceed(rpcTxResult)) {
      ChainCommand.returnTxFail(rpcTxResult)
    } else if (ChainCommand.getTxBooleanRes(rpcTxResult)) {
      Success("This transaction has been broadcast successfully, the transaction hash is " + hash)
    } else Success("This transaction failed to broadcast, please check the network.")
  }

}


