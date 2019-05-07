package com.apex.cli

import com.apex.consensus.{RegisterData, WitnessInfo}
import com.apex.core.{OperationType, TransactionType}
import com.apex.crypto.{BinaryData, FixedNumber, UInt256}
import com.apex.proposal.{ProposalData, ProposalType, ProposalVoteData}
import com.apex.vm.DataWord
import play.api.libs.json.JsValue
import play.api.libs.json._

class ProposalCommand extends CompositeCommand {
  override val cmd: String = "proposal"
  override val description: String = "Operate proposal"
  override val composite: Boolean = true

  // AP1xWDozWvuVah1W86DKtcWzdw1LqRxiSfr
  val proposalAddr = DataWord.of("0000000000000000000000000000000000000000000000000000000000000103")

  // AP1xWDozWvuVah1W86DKtcWzdw1LqdV35rk
  val proposalVoteAddr = DataWord.of("0000000000000000000000000000000000000000000000000000000000000104")

  override val subCommands: Seq[Command] = Seq(
    new NewProposalCommand,
    new VoteProposalCommand,
    new VoteListCommand,
    new ProposalListCommand
  )

  private class NewProposalCommand extends Command {
    override val cmd = "new"
    override val description = "Create a new proposal"

    override val paramList: ParameterList = ParameterList.create(
      new NicknameParameter("from", "from", "The account where the asset come from. Omit it if you want to send your tokens to the default account in the active wallet.", true),
      new IntParameter("para", "para", "The parameter you proposal to change."),
      new IntParameter("time", "time", "The proposal apply time (UTC seconds)"),
      new StringParameter("content", "content", "The proposal data."),
      new GasPriceParameter("gasPrice", "gasPrice", "The price of gas that the transaction / contract is willing to pay.")
    )

    override def execute(params: List[String]): Result = {
      try {
        val checkResult = Account.checkWalletStatus
        if (!checkResult.isEmpty) InvalidParams(checkResult)
        else {
          WalletCache.reActWallet
          var from = WalletCache.getActivityWallet().implyAccount
          if (params.size / 2 == paramList.params.size) from = paramList.params(0).asInstanceOf[NicknameParameter].value

          if (!Account.checkAccountStatus(from)) InvalidParams("from account not exists, please type a different one")
          else {
            val fromHash = Account.getAccount(from).getPrivKey().publicKey.pubKeyHash
            val para = paramList.params(1).asInstanceOf[IntParameter].value
            val time = paramList.params(2).asInstanceOf[IntParameter].value
            val content = paramList.params(3).asInstanceOf[StringParameter].value
            val price = paramList.params(4).asInstanceOf[GasPriceParameter].value
            val gasPrice = Util.calcGasPrice(price)

            val txData = ProposalData(ProposalType(para), time.longValue() * 1000, FixedNumber.fromDecimal(BigDecimal(content)).toBytes).toBytes
            val tx = Util.buildTx(TransactionType.Call, from, proposalAddr.toUInt160, FixedNumber.Zero, txData, gasPrice = gasPrice)
            val rpcTxResult = Util.sendTx(tx)
            printRes(rpcTxResult, tx.id())
          }
        }
      } catch {
        case e: Throwable => Error(e)
      }
    }
  }

  private class VoteProposalCommand extends Command {
    override val cmd = "vote"
    override val description = "Vote a proposal"

    override val paramList: ParameterList = ParameterList.create(
      new NicknameParameter("from", "from", "The account where the asset come from. Omit it if you want to send your tokens to the default account in the active wallet.", true),
      new StringParameter("pid", "pid", "The proposal ID/TXID"),
      new IntParameter("agree", "agree", "1 -> agree,   0 -> disagree"),
      new GasPriceParameter("gasPrice", "gasPrice", "The price of gas that the transaction / contract is willing to pay.")
    )

    override def execute(params: List[String]): Result = {
      try {
        val checkResult = Account.checkWalletStatus
        if (!checkResult.isEmpty) InvalidParams(checkResult)
        else {
          WalletCache.reActWallet
          var from = WalletCache.getActivityWallet().implyAccount
          if (params.size / 2 == paramList.params.size) from = paramList.params(0).asInstanceOf[NicknameParameter].value

          if (!Account.checkAccountStatus(from)) InvalidParams("from account not exists, please type a different one")
          else {
            val fromHash = Account.getAccount(from).getPrivKey().publicKey.pubKeyHash
            val pid = paramList.params(1).asInstanceOf[StringParameter].value
            val agree = paramList.params(2).asInstanceOf[IntParameter].value
            val price = paramList.params(3).asInstanceOf[GasPriceParameter].value
            val gasPrice = Util.calcGasPrice(price)

            val txData = ProposalVoteData(UInt256.fromBytes(BinaryData(pid)), agree == 1).toBytes
            val tx = Util.buildTx(TransactionType.Call, from, proposalVoteAddr.toUInt160, FixedNumber.Zero, txData, gasPrice = gasPrice)
            val rpcTxResult = Util.sendTx(tx)
            printRes(rpcTxResult, tx.id())
          }
        }
      } catch {
        case e: Throwable => Error(e)
      }
    }
  }

  private class VoteListCommand extends Command {
    override val cmd = "voteList"
    override val description = "List all current votes"

    override val paramList: ParameterList = ParameterList.create(
    )

    override def execute(params: List[String]): Result = {
      try {
        val result = RPC.post("getAllProposalVote", "{}")
        println(Json.prettyPrint(result))

        Success("")
      } catch {
        case e: Throwable => Error(e)
      }
    }
  }

  private class ProposalListCommand extends Command {
    override val cmd = "list"
    override val description = "List all current proposals"

    override val paramList: ParameterList = ParameterList.create(
    )

    override def execute(params: List[String]): Result = {
      try {
        val result = RPC.post("getAllProposal", "{}")
        println(Json.prettyPrint(result))

        Success("")
      } catch {
        case e: Throwable => Error(e)
      }
    }
  }

  private def printRes(rpcTxResult: JsValue, hash: UInt256): Result = {

    if (!ChainCommand.checkTxSucceed(rpcTxResult)) {
      ChainCommand.returnTxFail(rpcTxResult)
    } else if (ChainCommand.getTxBooleanRes(rpcTxResult)) {
      Success("This transaction has been broadcast successfully, the transaction hash is " + hash)
    } else Success("This transaction failed to broadcast, please check the network.")
  }


}