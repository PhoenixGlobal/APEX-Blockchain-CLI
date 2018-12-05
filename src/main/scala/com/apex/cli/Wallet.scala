package com.apex.cli

import java.io._
import java.nio.file.{Files, Paths}
import java.util.Calendar
import com.apex.crypto.Crypto
import lc.kra.system.keyboard.GlobalKeyboardHook
import scala.collection.mutable
import scala.io.Source
import scala.util.control.Breaks

class Wallet(val n :String, val p : Array[Byte], val accounts: Seq[Account]) extends com.apex.common.Serializable {

    def serialize(os: DataOutputStream) = {
      import com.apex.common.Serializable._
      os.writeString(n)
      os.writeByteArray(p)
      os.writeSeq(accounts)
    }
  }

  object Wallet {
    var Default: Wallet = null

    def deserialize(is: DataInputStream): Wallet = {
      import com.apex.common.Serializable._
      val n = is.readString()
      val keys = is.readByteArray()
      val accounts = is.readSeq(Account.deserialize)
      new Wallet(n, keys, accounts)
    }

    def fromBytes(data: Array[Byte]): Wallet = {
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
    walletCaches.get(n).get
  }

  def getActivityWallet(): WalletCache = {
    get(activityWallet)
  }

  def isExist(n:String):Boolean={
    if(get(n) == null) false
    else true
  }

  def remove(n:String){
    walletCaches.remove(n)
  }

  def size():Int={
    walletCaches.size
  }

  def checkTime(): Boolean ={
    val walletCache = get(WalletCache.activityWallet)
    val between = Calendar.getInstance().getTimeInMillis - walletCache.lastModify
    val minute = between / 1000 / 60
    /*val hour = between / 1000 / 3600
    val day = between / 1000 / 3600 / 24
    val year = between / 1000 / 3600 / 24 / 365*/

    if(minute < 1) true
    else false
  }

  def newWalletCache(wallet: Wallet): mutable.HashMap[String, WalletCache] ={

    if(walletCaches.contains(wallet.n)){
      val walletCache = walletCaches.get(wallet.n).get
      walletCache.lastModify = Calendar.getInstance().getTimeInMillis

      walletCaches.put(wallet.n, walletCache)
    }else {
      walletCaches.put(wallet.n, new WalletCache(wallet.n, wallet.p, accounts = wallet.accounts))
    }
    setActivate(wallet.n)

    walletCaches
  }

  def setActivate(n:String): Unit ={

    for (key <- walletCaches.keys) {
      val walletCache = walletCaches.get(key).get
      if(!walletCache.accounts.isEmpty) walletCache.implyAccount =  walletCache.accounts(0).n
      if(n.equals(key)){
        walletCache.activate = true
        activityWallet = key
      }else{
        walletCache.activate = false
      }
    }
  }

  val filePath = System.getProperty("user.home")+"\\cli_wallet\\"

  def fileExist(name:String):Boolean={
    val path =  filePath + name + ".json"

    Files.exists(Paths.get(path))
  }

  def readWallet(name:String):String={
    val path = filePath + name + ".json"
    val file = Source.fromFile(path)
    val walletContent = file.getLines.mkString
    file.close()
    walletContent
  }

  def writeWallet(name:String, key:Array[Byte], accounts:Seq[Account]): Wallet ={

    val file = new File(filePath)
    // 判断文件夹是否存在，不存在则创建
    if  (!file.exists()  && !file.isDirectory()) {
      file .mkdir()
      val exportFile = new File(filePath+"export")
      exportFile.mkdir()
    }

    val path = filePath + name + ".json"

    val wallet = new Wallet(name, key, accounts)

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

    wallet
  }

  def exportAccount(privkey: String, fileName : String): Unit ={
    val path = filePath +"export\\" + fileName

    val writer = new PrintWriter(new File(path ))

    writer.write(privkey)
    writer.close()
  }
}

class WalletCommand extends CompositeCommand {
  override val subCommands: Seq[Command] = Seq(
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

class WalletCreateCommand extends Command {

  override val cmd: String = "create"
  override val description: String = "create a new wallet"

  private val hook = new GlobalKeyboardHook

  override val paramList: ParameterList = ParameterList.create(
      new NicknameParameter("name", "n"),
      new PasswordParameter("password", "p")
  )

  override def execute(params: List[String]): Result = {

    try {

      val name = paramList.params(0).asInstanceOf[NicknameParameter].value
      val password = paramList.params(1).asInstanceOf[PasswordParameter].value

      if (WalletCache.fileExist(name)) {
        InvalidParams("Wallet [" + name + "] already exists, please type a different name")
      }else{
        val key = Crypto.sha256(password.getBytes("UTF-8"))

        val wallet = WalletCache.writeWallet(name, key, Seq.empty)
        WalletCache.newWalletCache(wallet)
        Success("wallet create success\n")
      }
    } catch {
      case e: Throwable => Error(e)
    }
  }

}

class WalletLoadCommand extends Command {

  override val cmd: String = "load"
  override val description: String = "load an existed wallet"
  val base64decoder = new sun.misc.BASE64Decoder

  override val paramList: ParameterList = ParameterList.create(
    new NicknameParameter("name", "n"),
    new PasswordParameter("password", "p")
  )

  override def execute(params: List[String]): Result = {

    val name = paramList.params(0).asInstanceOf[NicknameParameter].value
    val inputPwd = paramList.params(1).asInstanceOf[PasswordParameter].value

    if (!WalletCache.fileExist(name)) {
      InvalidParams("Wallet [" + name + "] does not exist\n")
    }else{

      // 获取文件内容
      val walletContent = WalletCache.readWallet(name)

      // 加密用户输入密码，并解密文件
      val key = Crypto.sha256(inputPwd.getBytes("UTF-8"))
      val iv : Array[Byte] = new Array(16)
      key.copyToArray(iv,0,16)

      var dec : Array[Byte] = new Array[Byte](1000)
      try{
        // 解密文件内容
        dec = Crypto.AesDecrypt(base64decoder.decodeBuffer(walletContent), key, iv)
      }catch {
        case e: Throwable =>  InvalidParams("Invalid password\n")
      }
      // 将对象反序列化
      val wallet = Wallet.fromBytes(dec)

      WalletCache.newWalletCache(wallet)
      Success("wallet load success\n")
    }
  }
}

class WalletCloseCommand extends Command {

  override val cmd: String = "close"
  override val description: String = "Close a loaded wallet"

  override val paramList: ParameterList = ParameterList.create(
    new NicknameParameter("name", "n")
  )

  override def execute(params: List[String]): Result = {

    val name = paramList.params(0).asInstanceOf[NicknameParameter].value

    if(!WalletCache.isExist(name))  InvalidParams("Wallet [" + name + "] have not loaded, type \"wallet list\" to see all loaded wallet.")
    else{
      WalletCache.remove(name)
      Success("wallet remove success\n")
    }
  }
}

class WalletActivateCommand extends Command {

  override val cmd: String = "activate"
  override val description: String = "Activate a candidate wallet. Use this command to switch amoung different wallets"

  override val paramList: ParameterList = ParameterList.create(
    new NicknameParameter("name", "n")
  )

  override def execute(params: List[String]): Result = {

    val name = paramList.params(0).asInstanceOf[NicknameParameter].value

    // 判断要激活的钱包是否存在
    if(!WalletCache.isExist(name)) InvalidParams("Wallet [" + name + "] have not loaded, type \"wallet list\" to see all loaded wallet.")
    else{
      // 设置钱包的状态
      WalletCache.setActivate(name)
      Success("wallet activate success\n")
    }
  }
}

class WalletActCommand extends WalletActivateCommand {
  override val cmd: String = "act"
}

class WalletListCommand extends Command {

  override val cmd: String = "list"
  override val description: String = "List all candidate wallet"

  override def execute(params: List[String]): Result = {

    WalletCache.walletCaches.values.foreach{i =>
      print(i.n)
      if(i.activate) print(" +")
      println("")
    }

    Success("wallet list successn\n")
  }
}