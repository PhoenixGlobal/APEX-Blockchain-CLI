package com.apex.cli

import java.io._
import java.util.Calendar
import com.apex.cli.Account.checkWalletStatus
import com.apex.crypto.Ecdsa.PrivateKey
import com.apex.crypto.{Base58Check, BinaryData, Crypto, FixedNumber, UInt256}
import play.api.libs.json.JsValue
import scala.util.parsing.json._

/*
 * Copyright  2018 APEX Technologies.Co.Ltd. All rights reserved.
 *
 * FileName: Account.scala
 *
 * @author: whitney.wei@chinapex.com: 18-12-10 @version: 1.0
 */
class Account(var n : String, var pri : String, var address: String) extends com.apex.common.Serializable{
  override def serialize(os: DataOutputStream): Unit = {
    import com.apex.common.Serializable._
    os.writeString(n)
    os.writeString(pri)
    os.writeString(address)
  }

  def getPrivKey(): PrivateKey = {
    PrivateKey.fromWIF(pri).get
  }

  def generateNewPrivKey() = {
    val key = new PrivateKey(BinaryData(Crypto.randomBytes(32)))
    pri = key.toWIF
    address = key.publicKey.address
  }

  def importPrivKeyFromWIF(wif: String): Boolean = {
    val key = getPrivKeyFromWIF(wif)
    if (key != None) {
      pri = wif
      true
    }
    else
      false
  }

  def getPrivKeyFromWIF(wif: String): Option[Array[Byte]] = {
    val decode = Base58Check.decode(wif).getOrElse(Array[Byte]())
    if (decode.length == 34) {
      // 1 bytes prefix + 32 bytes data + 1 byte 0x01 (+ 4 bytes checksum)
      if (decode(33) == 0x01.toByte) {
        Some(decode.slice(1, 33))
      } else {
        None
      }
    } else {
      None
    }
  }
}

object Account {
  var Default: Account = new Account("","", "")
  def deserialize(is: DataInputStream): Account = {
    import com.apex.common.Serializable._
    val n = is.readString
    val pri = is.readString
    val address = is.readString
    new Account(n, pri, address)
  }

  def fromBytes(data: Array[Byte]): Account = {
    val bs = new ByteArrayInputStream(data)
    val is = new DataInputStream(bs)
    deserialize(is)
  }

    def newAccount(n : String): Account ={
      val account = new Account(n, "", "")
      account.generateNewPrivKey()
      account
  }

  def checkWalletStatus: String = {
    var checkResult = ""
    if (WalletCache.size() < 1) {
      checkResult = "please load Wallet." // , type "wallet list" to see all loaded wallet
    } else if (WalletCache.activityWallet.isEmpty || !WalletCache.checkTime()) {
      checkResult = "please active Wallet, use \"wallet activate\" command to activate it."
    }
    checkResult
  }

  def checkAccountStatus(alias:String = "", address:String = ""): Boolean = {
    if(alias != null && !alias.isEmpty && WalletCache.getActivityWallet().accounts.groupBy(_.n).contains(alias)) true
    else if(address  != null && !address.isEmpty && WalletCache.getActivityWallet().accounts.groupBy(_.address).contains(address)) true
    else false
  }

  def checkAccountNotExists(alias:String = "", address:String=""): String = {
    var checkResult = checkWalletStatus
    if(!checkResult.isEmpty) checkResult
    else if(alias != null && !alias.isEmpty && !WalletCache.getActivityWallet().accounts.groupBy(_.n).keySet.contains(alias))
      checkResult = "account alias [" + alias + "] not exists, please type a different alias"
    else if(address  != null && !address.isEmpty && !WalletCache.getActivityWallet().accounts.groupBy(_.address).keySet.contains(address))
      checkResult = "account address [" + address + "] not exists, please type a different address"

    checkResult
  }

  def checkAccountExists(alias:String = "", address:String=""): String = {
    var checkResult = checkWalletStatus
    if(!checkResult.isEmpty) checkResult
    else if(alias != null && !alias.isEmpty && WalletCache.getActivityWallet().accounts.groupBy(_.n).keySet.contains(alias))
      checkResult = "account alias [" + alias + "] already exists, please type a different alias"
    else if(address != null && !address.isEmpty && WalletCache.getActivityWallet().accounts.groupBy(_.address).keySet.contains(address))
      checkResult = "account address [" + address + "] already exists, please type a different address"

    checkResult
  }

  def addAccount(alias:String): Account ={
    val account = Account.newAccount(alias)
    createAccountCache(account)
    account
  }

  def createAccountCache(account: Account) = {

    // 获取活跃钱包
    val walletCache = WalletCache.getActivityWallet()

    walletCache.accounts = walletCache.accounts.+:(account)
    walletCache.implyAccount = account.n
    walletCache.lastModify = Calendar.getInstance().getTimeInMillis
    // 写入缓存值
//    WalletCache.walletCaches.put(WalletCache.activityWallet, walletCache)
  }

  def delAccount(alias:String = "", address:String = ""){

    val walletCache = WalletCache.getActivityWallet()

    // 判断根据什么参数删除
    if(alias!=null && !alias.isEmpty) walletCache.accounts = walletCache.accounts.filter(!_.n.contains(alias))
    else walletCache.accounts = walletCache.accounts.filter(!_.address.contains(address))

    // 修改缓存信息
    if(walletCache.implyAccount.equals(alias)) walletCache.implyAccount = ""
    walletCache.lastModify = Calendar.getInstance().getTimeInMillis

    // 将缓存写入文件
    WalletCache.writeActWallet
  }

  def getAccount(alias:String = "", address:String = ""): Account ={

    if(alias!=null && !alias.isEmpty) WalletCache.getActivityWallet().accounts.groupBy(_.n).get(alias).get(0)
    else WalletCache.getActivityWallet().accounts.groupBy(_.address).get(address).get(0)
  }

  def modifyAccount(alias:String, to:String): Unit ={

    // 获取账户信息
    val account = getAccount(alias)
    // 修改账户名
    account.n = to

    // 获取缓存中账户的信息
    val walletCache = WalletCache.getActivityWallet()
    // 设置账户信息
    walletCache.implyAccount = to
    walletCache.lastModify = Calendar.getInstance().getTimeInMillis

    // 写入文件账户信息值
    WalletCache.writeActWallet
  }

  def implyAccount(account:Account): Unit ={

    val walletCache = WalletCache.getActivityWallet()
    walletCache.implyAccount = account.n
    walletCache.lastModify = Calendar.getInstance().getTimeInMillis
  }

  def getbalance(address :String) = {
    // 调用查询余额
    val rpcResult = RPC.post("showaccount", s"""{"address":"${address}"}""")
    // 转换查询结果
    val showaccount = JSON.parseFull(rpcResult.toString())
    // 申明余额变量
    var balance: String = FixedNumber.Zero.toString()
    if (showaccount != None && !(rpcResult \ "balance").isEmpty) balance = regJson(showaccount).get("balance").get.toString
    balance
  }

  def getResultBalance(rpcResult :JsValue) = {
    // 转换查询结果
    val showaccount = JSON.parseFull(rpcResult.toString())
    // 申明余额变量
    var balance: String = FixedNumber.Zero.toString()
    if (showaccount != None && !(rpcResult \ "balances").isEmpty) balance = regJson(showaccount).get("balance").get.toString
    balance
  }

  private def regJson(json: Option[Any]) = json match {
    case Some(map: Map[String, Any]) => map
    //      case None => "erro"
    //      case other => "Unknow data structure : " + other
  }
}

class AccountCommand extends CompositeCommand {
  override val cmd: String = "account"
  override val description: String = "Operate accounts of current wallet"
  override val composite: Boolean = true

  override val subCommands: Seq[Command] = Seq(
  new CreateAccountCommand,
  new ImportCommand,
  new ExportCommand,
  new DeleteCommand,
  new RemoveCommand,
  new RenameCommand,
  new ShowCommand,
  new ImplyCommand,
  new AccountListCommand
  )
}

class CreateAccountCommand extends Command {

  override val cmd: String = "new"
  override val description: String = "Add new account to current wallet"

  override val paramList: ParameterList = ParameterList.create(
    new NicknameParameter("alias", "a","alias of account")
  )

  override def execute(params: List[String]): Result = {

    try{

      val alias = paramList.params(0).asInstanceOf[NicknameParameter].value

      // 账户校验
      val checkResult = Account.checkAccountExists(alias)
      if(!checkResult.isEmpty) InvalidParams(checkResult)
      else{

        val account = Account.addAccount(alias)

        WalletCache.writeActWallet

        Success("success, address："+account.address+"\n")
      }
    } catch {
      case e: Throwable => Error(e)
    }
  }
}

class DeleteCommand extends Command {

  override val cmd: String = "delete"
  override val description: String = "Delete one account from current wallet"

  override val paramList: ParameterList = ParameterList.create(
    new NicknameParameter("alias", "a",
      "The alias of account. Use either this param or \"address\", If both give, the front one make sense.", true, true),
    new AddressParameter("address", "address",
      "The address of account. Use either this param or \"a\", If both give, the front one make sense.", true, true)
  )

  override def execute(params: List[String]): Result = {
    try{

      val alias = paramList.params(0).asInstanceOf[NicknameParameter].value
      val address = paramList.params(1).asInstanceOf[AddressParameter].value

      val checkResult = Account.checkAccountNotExists(alias, address)
      if(!checkResult.isEmpty) InvalidParams(checkResult)
      else{

        Account.delAccount(alias, address)
        Success("delete success\n")
      }
    } catch {
      case e: Throwable => Error(e)
    }
  }
}

class RemoveCommand extends DeleteCommand{
  override val cmd: String = "remove"
  override val description: String = "Same to \"delete\" command"
}

class RenameCommand extends Command {

  override val cmd: String = "rename"
  override val description: String = "Change the alias of one account within current wallet"

  override val paramList: ParameterList = ParameterList.create(
    new NicknameParameter("alias", "a","The alias of account."),
    new NicknameParameter("to", "to","The new alias of account.")
  )

  override def execute(params: List[String]): Result = {

    try{
      val alias = paramList.params(0).asInstanceOf[NicknameParameter].value
      val to = paramList.params(1).asInstanceOf[NicknameParameter].value


      // 校验钱包不存在
      val aliasCheckResult = Account.checkAccountNotExists(alias)
      // 校验钱包存在
      val toCheckResult = Account.checkAccountExists(to)

      if(!aliasCheckResult.isEmpty) InvalidParams(aliasCheckResult)
      if(!toCheckResult.isEmpty) InvalidParams(toCheckResult)
      else{
        // 获取账户信息
        val account = Account.modifyAccount(alias, to)

        Success("rename success\n")
      }
    } catch {
      case e: Throwable => Error(e)
    }

  }
}

class ShowCommand extends Command {

  override val cmd: String = "show"
  override val description: String = "Show the status of account"

  override val paramList: ParameterList = ParameterList.create(
    new NicknameParameter("alias", "a",
      "The alias of account. Use either this param or \"address\", If both give, the front one make sense.", true, true),
    new AddressParameter("address", "address",
      "The address of account. Use either this param or \"a\", If both give, the front one make sense.", true, true)
  )

  override def execute(params: List[String]): Result = {

    try{
      val alias = paramList.params(0).asInstanceOf[NicknameParameter].value
      val address = paramList.params(1).asInstanceOf[AddressParameter].value

      // 校验钱包不存在
      val checkResult = Account.checkAccountNotExists(alias)
      if(!checkResult.isEmpty) InvalidParams(checkResult)
      else{

        val account = Account.getAccount(alias, address)
        println(account.n + " -- " + account.address + " -- " + Account.getbalance(account.address))
        Success("show success\n")
      }
    } catch {
      case e: Throwable => Error(e)
    }
  }
}

class ImplyCommand extends Command {

  override val cmd: String = "imply"
  override val description: String = "Set account as default account in the wallet"

  override val paramList: ParameterList = ParameterList.create(
    new NicknameParameter("alias", "a",
      "The alias of account. Use either this param or \"address\", If both give, the front one make sense.", true, true),
    new AddressParameter("address", "address",
      "The address of account. Use either this param or \"a\", If both give, the front one make sense.", true, true)
  )

  override def execute(params: List[String]): Result = {

    try{
      val alias = paramList.params(0).asInstanceOf[NicknameParameter].value
      val address = paramList.params(1).asInstanceOf[AddressParameter].value
      // 校验钱包不存在
      val checkResult = Account.checkAccountNotExists(alias)
      if(!checkResult.isEmpty) InvalidParams(checkResult)
      else{
        Account.implyAccount(Account.getAccount(alias, address))
        Success("imply success\n")
      }
    } catch {
      case e: Throwable => Error(e)
    }
  }
}

class AccountListCommand extends Command {

  override val cmd: String = "list"
  override val description: String = "List all accounts of current wallet"

  override def execute(params: List[String]): Result = {

    try {
      if (checkWalletStatus.isEmpty && WalletCache.getActivityWallet() != null) {
        WalletCache.getActivityWallet().accounts.foreach { i =>
          // 申明余额变量
          var balance: String = Account.getbalance(i.address)

          print(i.n + " -- " + i.address + " -- " + balance)
          if (i.n.equals(WalletCache.getActivityWallet().implyAccount)) print(" +")
          println("")
        }
        WalletCache.reActWallet
      }

      Success("account list success\n")
    } catch {
      case e: Throwable => Error(e)
    }
  }
}

class ImportCommand extends Command {

  override val cmd: String = "import"
  override val description: String = "Import account to current wallet"

  override val paramList: ParameterList = ParameterList.create(
    new StringParameter("key", "key","Pivate key"),
    new NicknameParameter("alias", "a","alias of account")
  )

  override def execute(params: List[String]): Result = {

    try{

      val key = paramList.params(0).asInstanceOf[StringParameter].value
      val alias = paramList.params(1).asInstanceOf[NicknameParameter].value

      // 判断用户名是否存在
      val checkResult = Account.checkAccountExists(alias)
      if(!checkResult.isEmpty) InvalidParams(checkResult)
      else{
        val account = Account.Default
        if (account.importPrivKeyFromWIF(key)) {
          account.n = alias
          val importAddress = account.getPrivKey().publicKey.address

          // 根据地址查询
          if(!Account.checkAccountStatus(address = importAddress)){

            // 设置缓存
            account.address = importAddress
            Account.createAccountCache(account)

            // 写入到文件中
            WalletCache.writeActWallet

            Success("\nimport success\n")
          }else InvalidParams("account key [" + key + "] already exists, please type a different key\n")

        } else InvalidParams("key error\n")
      }
    } catch {
      case e: Throwable => Error(e)
    }
  }
}

class ExportCommand extends Command {

  override val cmd: String = "export"
  override val description: String = "Export one account within current wallet"

  override val paramList: ParameterList = ParameterList.create(
    new NicknameParameter("alias", "a","alias of account"),
    new StringParameter("file", "file",
      "The file which the private key is wrote to.Omit it if you want to print the private key on the screen.", true)
  )

  override def execute(params: List[String]): Result = {

    try{
      val alias = paramList.params(0).asInstanceOf[NicknameParameter].value
      val file = paramList.params(1).asInstanceOf[StringParameter].value

      val checkResult = Account.checkWalletStatus
      if(!checkResult.isEmpty) InvalidParams(checkResult)
      else{
        // 显示私钥
        val account = Account.getAccount(alias)

        if(file == null)
          println("pri ==> "+account.pri)
        else{
          WalletCache.exportAccount(account.pri, file)
        }
        WalletCache.reActWallet

        Success("export success\n")
      }
    } catch {
      case e: Throwable => Error(e)
    }
  }
}
