/*
 * Copyright  2018 APEX Technologies.Co.Ltd. All rights reserved.
 *
 * FileName: Wallet.scala
 *
 * @author: shan.huang@chinapex.com: 2018-07-27 下午4:06@version: 1.0
 */

package com.apex.cli

import java.io.FileWriter
import com.apex.crypto.{Base58Check, BinaryData, Crypto}
import com.apex.crypto.Ecdsa.PrivateKey
import play.api.libs.json._
import play.api.libs.json.Reads._
import play.api.libs.functional.syntax._

case class Wallet(val name: String,
                  var address: String,
                  var privKey: String,
                  val version: String) {

  //private val privKeys = Set.empty[PrivateKey]

  def getPrivKey(): PrivateKey = {
    PrivateKey.fromWIF(privKey).get
  }

  def generateNewPrivKey() = {
    val key = new PrivateKey(BinaryData(Crypto.randomBytes(32)))
    privKey = key.toWIF
    address = key.publicKey.toAddress
  }

  def importPrivKeyFromWIF(wif: String): Boolean = {
    val key = getPrivKeyFromWIF(wif)
    if (key != None) {
      privKey = wif
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

object Wallet {
  var Default: Wallet = null

  val fileName = "wallet.json"

  implicit val walletWrites = new Writes[Wallet] {
    override def writes(o: Wallet): JsValue = {
      Json.obj(
        "name" -> o.name,
        "address" -> o.address,
        "privKey"  -> o.privKey,
        "version" -> o.version
      )
    }
  }

  implicit val walletReads: Reads[Wallet] = (
    (JsPath \ "name").read[String] and
      (JsPath \ "address").read[String] and
      (JsPath \ "privKey").read[String] and
      (JsPath \ "version").read[String]

    ) (Wallet.apply _)

  def load() = {

    import scala.io.Source

    Default = new Wallet("", "","", "0.1")
    Default.generateNewPrivKey()

    try {
      val walletJson = Json.parse(Source.fromFile(fileName).getLines().mkString)

      walletJson.validate[Wallet] match {
        case w: JsSuccess[Wallet] => {
          Default = w.get
          println(s"Open wallet file: $fileName")
        }
        case _: JsError => {
          println("error parse wallet file")
        }
      }
    }
    catch {
      case e: Throwable => {
        println(s"Created new wallet file: $fileName")
        save()
      }
    }
    save()
  }

  def save() = {
    val fw = new FileWriter(fileName)
    fw.write(Json.toJson(Default).toString)
    fw.close()
  }
}