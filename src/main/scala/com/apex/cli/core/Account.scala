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

// ---------账户功能 start
class AccountCommand extends NewCompositeCommand {
  override val subCommands: Seq[NewCommand] = Seq(
    new NewAccountCommand
  )

  override val cmd: String = "account"
  override val description: String = "Operate accounts of current wallet"
}

class NewAccountCommand extends NewCommand {

  override val cmd: String = "new"
  override val description: String = "Add new account to current wallet"

  override val paramList: NewParameterList = NewParameterList.create(
    new NicknameParameter("name", "n")
  )

  override def execute(params: List[String]): NewResult = {

    val name = paramList.params(0).asInstanceOf[NicknameParameter].value

    // 判断当前是否有活跃钱包
    if (WalletCache.size() < 1) {
      return NewSuccess("please load wallet")
    } else if (WalletCache.activityWallet.isEmpty) {
      return NewSuccess("please active Wallet, use \"wallet activate\" command to activate it.")
    }
    val walletCache = WalletCache.get(WalletCache.activityWallet)
    val path = "D:\\chain\\whitney\\" + walletCache.n + ".json"
    val account = WalletCache.addAccount(name)

    WalletCache.writeWallet(path, walletCache.n, walletCache.p, walletCache.accounts)

    NewSuccess("address："+account.address)
  }
}
// ---------账户功能 end