package com.apex.cli.core

import java.io.{ByteArrayInputStream, DataInputStream, DataOutputStream}
import java.util.Calendar

import com.apex.cli.{InvalidParams, Success}
import com.apex.cli.core.WalletCache.{activityWallet, walletCaches}
import com.apex.crypto.{Base58Check, BinaryData, Crypto}
import com.apex.crypto.Ecdsa.PrivateKey


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
      return account
  }

  def checkWalletStatus: String = {
    if (WalletCache.size() < 1) {
      return "please load Wallet, type \"wallet list\" to see all loaded wallet."
    } else if (WalletCache.activityWallet.isEmpty || !WalletCache.checkTime()) {
      return "please active Wallet, use \"wallet activate\" command to activate it."
    }
    return ""
  }

  def checkAccountStatus(alias:String = "", address:String=""): String = {
    val checkResult = checkWalletStatus
    if(!checkResult.isEmpty) return checkResult
    else if(!alias.isEmpty && WalletCache.getActivityWallet().accounts.groupBy(_.n).keySet.contains(alias))
      return "account alias [" + alias + "] already exists, please type a different alias"
    else if(!address.isEmpty && WalletCache.getActivityWallet().accounts.groupBy(_.address).keySet.contains(address))
      return "account address [" + address + "] already exists, please type a different alias"

    return ""
  }

  def addAccount(alias:String): Account ={
    val account = Account.newAccount(alias)

    createAccountCache(account)
    return account
  }

  def createAccountCache(account: Account) = {

    // 获取活跃钱包
    val walletCache = WalletCache.getActivityWallet()

    val accounts = walletCache.accounts.+:(account)

    walletCache.accounts = accounts
    walletCache.implyAccount = account.n
    walletCache.lastModify = Calendar.getInstance().getTimeInMillis

    walletCaches.put(activityWallet, walletCache)
  }

  def delAccount(alias:String = "", address:String = ""){

    val walletCache = WalletCache.getActivityWallet()

    // 判断根据什么参数删除
    var accounts:Seq[Account] =  Seq[Account]()
    if(!alias.isEmpty) accounts = walletCache.accounts.filter(!_.n.contains(alias))
    else accounts = walletCache.accounts.filter(!_.address.contains(address))

    // 修改缓存信息
    walletCache.accounts = accounts
    if(walletCache.implyAccount.equals(alias)) walletCache.implyAccount = ""
    walletCache.lastModify = Calendar.getInstance().getTimeInMillis

    // 写入文件账户信息值
    WalletCache.writeWallet(walletCache.n, walletCache.p, walletCache.accounts)

    // 写入缓存值
    walletCaches.put(activityWallet, walletCache)
  }

  def getAccount(alias:String = "", address:String = ""): Account ={

    if(!alias.isEmpty) return WalletCache.getActivityWallet().accounts.groupBy(_.n).get(alias).get(0)
    else return WalletCache.getActivityWallet().accounts.groupBy(_.address).get(address).get(0)
  }

  def modifyAccount(alias:String, to:String): Unit ={

    // 获取账户信息
    val account = getAccount(alias)
    // 修改账户名
    account.n = to

    // 获取缓存中账户的信息
    val walletCache = WalletCache.getActivityWallet()
    var accounts = walletCache.accounts.filter(!_.n.contains(alias))

    // 设置账户信息
    walletCache.accounts = accounts.+:(account)
    if(walletCache.implyAccount.equals(alias)) walletCache.implyAccount = to
    walletCache.lastModify = Calendar.getInstance().getTimeInMillis

    // 写入文件账户信息值
    WalletCache.writeWallet(walletCache.n, walletCache.p, walletCache.accounts)

    // 写入缓存
    walletCaches.put(activityWallet, walletCache)
  }

  def implyAccount(account:Account): Unit ={

    val walletCache = WalletCache.getActivityWallet()
    walletCache.implyAccount = account.n
    walletCache.lastModify = Calendar.getInstance().getTimeInMillis
    walletCaches.put(activityWallet, walletCache)
  }

}

class AccountCommand extends NewCompositeCommand {
  override val subCommands: Seq[NewCommand] = Seq(
    new NewAccountCommand,
    new ImportCommand,
    new ExportCommand,
    new DeleteCommand,
    new RemoveCommand,
    new RenameCommand,
    new ShowCommand,
    new ImplyCommand,
    new AccountListCommand
  )

  override val cmd: String = "account"
  override val description: String = "Operate accounts of current wallet"
}

class NewAccountCommand extends NewCommand {

  override val cmd: String = "new"
  override val description: String = "Add new account to current wallet"

  override val paramList: NewParameterList = NewParameterList.create(
    new NicknameParameter("alias", "a")
  )

  override def execute(params: List[String]): NewResult = {

    val alias = paramList.params(0).asInstanceOf[NicknameParameter].value

    // 账户校验
    val checkResult = Account.checkAccountStatus(alias)
    if(!checkResult.isEmpty) return NewInvalidParams(checkResult)

    val walletCache = WalletCache.getActivityWallet()

    val account = Account.addAccount(alias)

    WalletCache.writeWallet(walletCache.n, walletCache.p, walletCache.accounts)

    NewSuccess("success, address："+account.address+"\n")
  }
}

class DeleteCommand extends NewCommand {

  override val cmd: String = "delete"
  override val description: String = "Delete one account from current wallet"

  override val paramList: NewParameterList = NewParameterList.create(
    new NicknameParameter("alias", "a", true, true),
    new NewAddressParameter("address", "address", true, true)
  )

  override def execute(params: List[String]): NewResult = {

    val alias = paramList.params(0).asInstanceOf[NicknameParameter].value
    val address = paramList.params(1).asInstanceOf[NewAddressParameter].value

    val checkResult = Account.checkAccountStatus(alias, address)
    if(!checkResult.isEmpty) return NewInvalidParams(checkResult)

    // 从缓存中获取
    if(alias != null) Account.delAccount(alias = alias)
    else Account.delAccount(address = address)

    NewSuccess("delete success\n")
  }
}

class RemoveCommand extends DeleteCommand{
  override val cmd: String = "remove"
  override val description: String = "Same to \"delete\" command"
}

class RenameCommand extends NewCommand {

  override val cmd: String = "rename"
  override val description: String = "Change the alias of one account within current wallet"

  override val paramList: NewParameterList = NewParameterList.create(
    new NicknameParameter("alias", "a"),
    new NicknameParameter("to", "to")
  )

  override def execute(params: List[String]): NewResult = {

    val alias = paramList.params(0).asInstanceOf[NicknameParameter].value
    val to = paramList.params(1).asInstanceOf[NicknameParameter].value

    val checkResult = Account.checkAccountStatus(to)
    if(!checkResult.isEmpty) return NewInvalidParams(checkResult)

    // 获取账户信息
    val account = Account.modifyAccount(alias, to)

    NewSuccess("rename success\n")
  }
}

class ShowCommand extends NewCommand {

  override val cmd: String = "show"
  override val description: String = "Show the status of account"

  override val paramList: NewParameterList = NewParameterList.create(
    new NicknameParameter("alias", "a", true, true),
    new NewAddressParameter("address", "address", true, true)
  )

  override def execute(params: List[String]): NewResult = {

    val alias = paramList.params(0).asInstanceOf[NicknameParameter].value
    val address = paramList.params(1).asInstanceOf[NewAddressParameter].value

    val account = Account.getAccount(alias, address)

    println(account.n + "--"+account.pri)
    NewSuccess("show success\n")
  }
}

class ImplyCommand extends NewCommand {

  override val cmd: String = "imply"
  override val description: String = "Set account as default account in the wallet"

  override val paramList: NewParameterList = NewParameterList.create(
    new NicknameParameter("alias", "a", true, true),
    new NewAddressParameter("address", "address", true, true)
  )

  override def execute(params: List[String]): NewResult = {

    val alias = paramList.params(0).asInstanceOf[NicknameParameter].value
    val address = paramList.params(1).asInstanceOf[NewAddressParameter].value

    val account = Account.getAccount(alias, address)

    if(account != null) Account.implyAccount(account)

    NewSuccess("imply success\n")
  }
}

class AccountListCommand extends NewCommand {

  override val cmd: String = "list"
  override val description: String = "List all accounts of current wallet"

  override def execute(params: List[String]): NewResult = {

    println("implyAccount： "+WalletCache.getActivityWallet().implyAccount)

    WalletCache.getActivityWallet().accounts.foreach{i =>
      println(i.n +" -- " +i.address)
    }
    NewSuccess("account list\n")
  }
}

class ImportCommand extends NewCommand {

  override val cmd: String = "import"
  override val description: String = "Import account to current wallet"

  override val paramList: NewParameterList = NewParameterList.create(
    new NewStringParameter("key", "key"),
    new NicknameParameter("alias", "a")
  )

  override def execute(params: List[String]): NewResult = {

    val key = paramList.params(0).asInstanceOf[NewStringParameter].value
    val alias = paramList.params(1).asInstanceOf[NicknameParameter].value

    val account = Account.Default
    if (account.importPrivKeyFromWIF(key)) {
      account.n = alias
      account.address = account.getPrivKey().publicKey.address

      Account.createAccountCache(account)
    } else InvalidParams("key error\n")

    NewSuccess("import\n")
  }
}

class ExportCommand extends NewCommand {

  override val cmd: String = "export"
  override val description: String = "Export one account within current wallet"

  override val paramList: NewParameterList = NewParameterList.create(
    new NicknameParameter("alias", "a"),
    new NewStringParameter("file", "file")
  )

  override def execute(params: List[String]): NewResult = {



    NewSuccess("export\n")
  }
}
