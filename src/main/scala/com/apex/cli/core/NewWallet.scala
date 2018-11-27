package com.apex.cli.core

import java.io._
import java.nio.file.{Files, Paths}
import java.util.Calendar

import com.apex.common.LRUCache
import com.apex.crypto.{Base58Check, BinaryData, Crypto}
import com.apex.crypto.Ecdsa.PrivateKey

import collection.JavaConversions._
import scala.collection.mutable
import scala.io.Source
import scala.util.control.Breaks

class NewWallet(val n :String, val p : Array[Byte], val accounts: Seq[Account]) extends com.apex.common.Serializable {

    def serialize(os: DataOutputStream) = {
      import com.apex.common.Serializable._
      os.writeString(n)
      os.writeByteArray(p)
      os.writeSeq(accounts)
    }
  }

  object NewWallet {
    var Default: NewWallet = null

    def deserialize(is: DataInputStream): NewWallet = {
      import com.apex.common.Serializable._
      val n = is.readString()
      val keys = is.readByteArray()
      val accounts = is.readSeq(Account.deserialize)
      new NewWallet(n, keys, accounts)
    }

    def fromBytes(data: Array[Byte]): NewWallet = {
      val bs = new ByteArrayInputStream(data)
      val is = new DataInputStream(bs)
      deserialize(is)
    }
  }

class WalletCache(val n:String, val p : Array[Byte],
                  var lastModify:Long = Calendar.getInstance().getTimeInMillis,
                  var activate : Boolean = true, var implyAccount : String = "",
                  var accounts:Seq[Account] =  Seq[Account]()){

}

object WalletCache{

  val walletCaches : mutable.HashMap[String, WalletCache] = new mutable.HashMap[String,WalletCache]()
  val base64encoder = new sun.misc.BASE64Encoder
  var activityWallet:String = ""

  def get(n:String): WalletCache ={
    return walletCaches.get(n).get
  }

  def remove(n:String){
    walletCaches.remove(n)
  }

  def size():Int={
    return walletCaches.size
  }

  def checkTime(): Boolean ={
    val walletCache = get(WalletCache.activityWallet)
    val between = Calendar.getInstance().getTimeInMillis - walletCache.lastModify
    val second = between / 1000
    /*val hour = between / 1000 / 3600
    val day = between / 1000 / 3600 / 24
    val year = between / 1000 / 3600 / 24 / 365*/

    if(second < 11) return true

    return false;
  }

  def newWalletCache(wallet: NewWallet): mutable.HashMap[String, WalletCache] ={

    if(walletCaches.contains(wallet.n)){
      val walletCache = walletCaches.get(wallet.n).get
      walletCache.lastModify = Calendar.getInstance().getTimeInMillis

      walletCaches.put(wallet.n, walletCache)
    }else {
      walletCaches.put(wallet.n, new WalletCache(wallet.n, wallet.p))
    }

    setActivate(wallet.n)
    return walletCaches
  }

  def setActivate(n:String): Unit ={

    for (key <- walletCaches.keys) {
      val walletCache = walletCaches.get(key).get
      if(n.equals(key)){
        walletCache.activate = true
        activityWallet = key
      }else{
        walletCache.activate = false
      }
    }
  }

  def addAccount(n:String): Account ={
    val account = Account.newAccount(n)

    val walletCache = walletCaches.get(activityWallet).get
    val accounts = walletCache.accounts.+:(account)

    walletCache.accounts = accounts
    walletCache.implyAccount = n
    walletCache.lastModify = Calendar.getInstance().getTimeInMillis

    walletCaches.put(activityWallet, walletCache)

    return account
  }

  def writeWallet(path:String, name:String, key:Array[Byte], accounts:Seq[Account]): NewWallet ={

    val wallet = new NewWallet(name, key, accounts)

    val bs = new ByteArrayOutputStream()
    val os = new DataOutputStream(bs)

    wallet.serialize(os)

    val iv : Array[Byte] = new Array(16)
    key.copyToArray(iv,0,16)

    // 加密用户输入的密码
    val encrypted1 = Crypto.AesEncrypt(bs.toByteArray, key, iv)

    val fw = new FileWriter(path)
    fw.write(base64encoder.encode(encrypted1))
    fw.close()

    return wallet
  }
}

class WalletCommand extends NewCompositeCommand {
  override val subCommands: Seq[NewCommand] = Seq(
    new WalletCreateCommand,
    new WalletLoadCommand,
    new WalletCloseCommand,
    new WalletActivateCommand,
    new WalletActCommand,
    new WalletListCommand
  )

  override val cmd: String = "wallet"
  override val description: String = "Operate a wallet, user accounts must add to one wallet before using it"
}

class WalletCreateCommand extends NewCommand {

  override val cmd: String = "create"
  override val description: String = "create a new wallet"

  override val paramList: NewParameterList = NewParameterList.create(
    new NicknameParameter("name", "n")
  )

  override def execute(params: List[String]): NewResult = {
    try {
      val name = paramList.params(0).asInstanceOf[NicknameParameter].value
      val path = "D:\\chain\\whitney\\" + name + ".json"

      val isexist = Files.exists(Paths.get(path))
      if (isexist) {
        return NewSuccess("Wallet [" + name + "] already exists, please type a different name")
      }

      var password, password2: String = ""

      val loop = new Breaks;
      loop.breakable{
        while (true) {
          println("Please enter your pwd : ")
          password = scala.io.StdIn.readLine()
          println("Please enter your pwd again: ")
          password2 = scala.io.StdIn.readLine()

          if (password.equals(password2)) loop.break
          else println("password error\n")
        }
      }

      val key = Crypto.sha256(password.getBytes("UTF-8"))

      val wallet = WalletCache.writeWallet(path, name, key, Seq[Account](Account.Default))
      WalletCache.newWalletCache(wallet)
      NewSuccess("wallet create success")
    } catch {
      case e: Throwable => NewError(e)
    }
  }

}

class WalletLoadCommand extends NewCommand {

  override val cmd: String = "load"
  override val description: String = "load an existed wallet"
  val base64decoder = new sun.misc.BASE64Decoder

  override val paramList: NewParameterList = NewParameterList.create(
    new NicknameParameter("name", "n")
  )

  override def execute(params: List[String]): NewResult = {

    val name = paramList.params(0).asInstanceOf[NicknameParameter].value
    val path = "D:\\chain\\whitney\\" + name + ".json"

    val isexist = Files.exists(Paths.get(path))
    if (!isexist) {
      return NewSuccess("Wallet [" + name + "] does not exist")
    }

    println("Please enter your pwd : ")
    val inputPwd = scala.io.StdIn.readLine()

    val file = Source.fromFile(path)
    val walletContent = file.getLines.mkString

    file.close()

    val key = Crypto.sha256(inputPwd.getBytes("UTF-8"))

    val iv : Array[Byte] = new Array(16)
    key.copyToArray(iv,0,16)

    val dec = Crypto.AesDecrypt(base64decoder.decodeBuffer(walletContent), key, iv)

    // 将对象反序列化
    val wallet = NewWallet.fromBytes(dec)

    WalletCache.newWalletCache(wallet)

    if(new String(wallet.p) == new String(key)){
      NewSuccess("wallet load success："+walletContent)
    }else NewSuccess("\nInvalid password\n")
  }
}

class WalletCloseCommand extends NewCommand {

  override val cmd: String = "close"
  override val description: String = "Close a loaded wallet"

  override val paramList: NewParameterList = NewParameterList.create(
    new NicknameParameter("name", "n")
  )

  override def execute(params: List[String]): NewResult = {

    val name = paramList.params(0).asInstanceOf[NicknameParameter].value

    val walletCache = WalletCache.get(name)
    if(walletCache == null){
      return NewSuccess("Wallet [" + name + "] does not exist")
    }

    WalletCache.remove(name)
    NewSuccess("\nInvalid password\n")
  }
}

class WalletActivateCommand extends NewCommand {

  override val cmd: String = "activate"
  override val description: String = "Activate a candidate wallet. Use this command to switch amount different wallets"

  override val paramList: NewParameterList = NewParameterList.create(
    new NicknameParameter("name", "n")
  )

  override def execute(params: List[String]): NewResult = {

    val name = paramList.params(0).asInstanceOf[NicknameParameter].value

    NewSuccess("\nInvalid password\n")
  }
}

class WalletActCommand extends WalletActivateCommand {
  override val cmd: String = "act"
}

class WalletListCommand extends NewCommand {

  override val cmd: String = "list"
  override val description: String = "List all candidate wallet"

  override def execute(params: List[String]): NewResult = {

    NewSuccess("")
  }
}