package com.apex.cli.core

import java.io.{ByteArrayInputStream, DataInputStream, DataOutputStream}
import com.apex.crypto.{Base58Check, BinaryData, Crypto}
import com.apex.crypto.Ecdsa.PrivateKey


class Account(val n : String, var pri : String, var address: String) extends com.apex.common.Serializable{
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

    val name = paramList.params(0).asInstanceOf[NicknameParameter].value

    if (WalletCache.size() < 1) {
      return NewSuccess("please load wallet")
    } else if (WalletCache.activityWallet.isEmpty || !WalletCache.checkTime()) {
      return NewSuccess("please active Wallet, use \"wallet activate\" command to activate it.")
    }
    val walletCache = WalletCache.get(WalletCache.activityWallet)
    val path = "D:\\chain\\whitney\\" + walletCache.n + ".json"
    val account = WalletCache.addAccount(name)

    WalletCache.writeWallet(path, walletCache.n, walletCache.p, walletCache.accounts)

    NewSuccess("address："+account.address)
  }
}

class ImportCommand extends NewCommand {

  override val cmd: String = "import"
  override val description: String = "Import account to current wallet"

  override val paramList: NewParameterList = NewParameterList.create(
    new NewPrivKeyParameter("key", "key"),
    new NicknameParameter("alias", "a")
  )

  override def execute(params: List[String]): NewResult = {

    NewSuccess("import")
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

    NewSuccess("export")
  }
}

class DeleteCommand extends NewCommand {

  override val cmd: String = "delete"
  override val description: String = "Delete one account from current wallet"

  override val paramList: NewParameterList = NewParameterList.create(
    new NicknameParameter("alias", "a", true),
    new NewAddressParameter("address", "address", true)
  )

  override def execute(params: List[String]): NewResult = {

    NewSuccess("delete")
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
    new NewAddressParameter("to", "to")
  )

  override def execute(params: List[String]): NewResult = {

    NewSuccess("rename")
  }
}

class ShowCommand extends NewCommand {

  override val cmd: String = "show"
  override val description: String = "Show the status of account"

  override val paramList: NewParameterList = NewParameterList.create(
    new NicknameParameter("alias", "a", true),
    new NewAddressParameter("address", "address", true)
  )

  override def execute(params: List[String]): NewResult = {

    NewSuccess("show")
  }
}

class ImplyCommand extends NewCommand {

  override val cmd: String = "imply"
  override val description: String = "Set account as default account in the wallet"

  override val paramList: NewParameterList = NewParameterList.create(
    new NicknameParameter("alias", "a", true),
    new NewAddressParameter("address", "address", true)
  )

  override def execute(params: List[String]): NewResult = {

    NewSuccess("imply")
  }
}

class AccountListCommand extends NewCommand {

  override val cmd: String = "list"
  override val description: String = "List all accounts of current wallet"

  override def execute(params: List[String]): NewResult = {

    NewSuccess("list")
  }
}