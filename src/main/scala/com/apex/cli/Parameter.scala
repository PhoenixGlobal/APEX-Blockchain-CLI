/*
 * Copyright  2018 APEX Technologies.Co.Ltd. All rights reserved.
 *
 * FileName: Parameter.scala
 *
 * @author: ruixiao.xiao@chinapex.com: 18-8-10 下午1:55@version: 1.0
 */

package com.apex.cli

import play.api.libs.json.{JsNumber, JsObject, JsString, JsValue}
import util.control.Breaks._

trait Parameter {
  val name: String
  val shortName: String
  val description: String
  // 是否可以跳过验证
  val halt : Boolean = false
  // 是否有可替代属性
  val replaceable : Boolean = false


  def toJson(): JsValue

  def validate(n: String, v: String, setEmpty : Boolean = false): Boolean

  protected def validateName(s: String): Boolean = {
//    s.equals(s"-$shortName") || s.equals(s"-$name")
    s.trim.equals(shortName) || s.trim.equals(name)
  }
}

class IntParameter(override val name: String, override val shortName: String, override val description: String="",
                   override val halt: Boolean = false, override val replaceable: Boolean = false) extends Parameter {
  var value: Integer = 0

  override def toJson: JsValue = JsNumber(value.toInt)

  override def validate(n: String, v: String, setEmpty : Boolean = false): Boolean = {
    if(setEmpty){
      value = null
      true
    } else
      validateName(n) && setValue(v)
  }

  private def setValue(s: String): Boolean = {
    try {
      value = s.toInt
      true
    } catch {
      case _: Throwable => false
    }
  }
}

class StringParameter(override val name: String, override val shortName: String, override val description: String="",
                      override val halt: Boolean = false, override val replaceable: Boolean = false) extends Parameter {
  var value: String = null

  override def toJson: JsValue = JsString(value)

  override def validate(n: String, v: String, setEmpty : Boolean = false): Boolean = {
    if(setEmpty){
      value = null
      true
    } else
      validateName(n) && setValue(v)
  }

  private def setValue(s: String): Boolean = {
    value = s
    true
  }
}

class IdParameter(override val name: String = "id", override val shortName: String = "id", override val description: String ="") extends Parameter {
  var value: String = null
  //  private val regex = """[0-9a-fA-F]32""".r

  override def toJson: JsValue = JsString(value)

  override def validate(n: String, v: String, setEmpty : Boolean = false): Boolean = {
    if(setEmpty){
      value = null
      true
    } else
      validateName(n) && setValue(v)
  }

  private def setValue(s: String): Boolean = {
    if (s.length == 64) {
      value = s
      true
    } else {
      false
    }
    //    s match {
    //      case regex(s) => {
    //        value = s
    //        true
    //      }
    //      case _ => false
    //    }
  }
}

class AddressParameter(override val name: String = "address", override val shortName: String = "address", override val description: String ="",
                       override val halt: Boolean = false, override val replaceable: Boolean = false) extends Parameter {
  var value: String = null

  override def toJson: JsValue = JsString(value)

  override def validate(n: String, v: String, setEmpty : Boolean = false): Boolean = {
    if(setEmpty){
      value = null
      true
    } else
      validateName(n) && setValue(v)
  }

  private def setValue(s: String): Boolean = {
    if (s.length == 35) {
      value = s
      true
    } else {
      false
    }
  }
}

class HelpParameter(override val name: String = "help", override val shortName: String = "h", override val description: String ="") extends Parameter {
  var value: String = null

  override def toJson: JsValue = JsString(value)

  override def validate(n: String, v: String, setEmpty : Boolean = false): Boolean = {
    if(setEmpty){
      value = null
      true
    } else
      validateName(n) && setValue(v)
  }

  private def setValue(s: String): Boolean = {
    false
  }
}

class NicknameParameter(override val name: String, override val shortName: String, override val description: String="",
                        override val halt: Boolean = false, override val replaceable: Boolean = false) extends Parameter {
  var value: String = null

  private val regex = """^(?![0-9]+$)(?![a-zA-Z]+$)[0-9A-Za-z]{1,12}$""".r

  override def toJson: JsValue = JsString(value)

  override def validate(n: String, v: String, setEmpty : Boolean = false): Boolean = {
    if(setEmpty){
      value = null
      true
    } else
      validateName(n) && setValue(v)
  }

  private def setValue(s: String): Boolean = {
    if(regex.pattern.matcher(s).matches()){
      value = s
      true
    }else false
  }
}

class PasswordParameter(override val name: String = "password", override val shortName: String = "p", override val description: String="") extends Parameter {
  var value: String = null
  private val regex = """^(?![0-9]+$)(?![a-zA-Z]+$)[0-9A-Za-z]{8,}$""".r

  override def toJson: JsValue = JsString(value)

  override def validate(n: String, v: String, setEmpty : Boolean = false): Boolean = {
    if(setEmpty){
      value = null
      true
    } else
      validateName(n) && setValue(v)
  }

  private def setValue(s: String): Boolean = {
    if(regex.pattern.matcher(s).matches()){
      value = s
      true
    }else false
  }
}

class PrivKeyParameter(override val name: String = "privkey", override val shortName: String = "privkey", override val description: String="",
                       override val halt: Boolean = false, override val replaceable: Boolean = false) extends Parameter {
  var value: String = null

  override def toJson: JsValue = JsString(value)

  override def validate(n: String, v: String, setEmpty : Boolean = false): Boolean = {
    if(setEmpty){
      value = null
      true
    } else
      validateName(n) && setValue(v)
  }

  private def setValue(s: String): Boolean = {
    if (s.length == 64) {
      value = s
      true
    } else {
      false
    }
  }
}


class AmountParameter(override val name: String = "amount", override val shortName: String = "amount", override val description: String="") extends Parameter {
  var value: BigDecimal = null

  override def toJson: JsValue = JsString(value.toString)

  override def validate(n: String, v: String, setEmpty : Boolean = false): Boolean = {
    if(setEmpty){
      value = null
      true
    } else
      validateName(n) && setValue(v)
  }

  private def setValue(s: String): Boolean = {
    try {
      value = BigDecimal(s)
      value.signum > 0
    } catch {
      case _: Throwable => false
    }
  }
}

abstract class ParameterList(val params: Seq[Parameter]) {
  def validate(list: List[String]): Boolean = {
    if (params == null || params.isEmpty) {
      true
    } else {
      validate(list, 0)
    }
  }

  def toJson(): String = {
    JsObject(
      params.map(p => p.name -> p.toJson)
    ).toString
  }

  override def toString: String = {
    params.map(p => s"-${p.shortName}").mkString(",")
  }

  protected def validate(list: List[String], i: Int): Boolean
}

object ParameterList {
  val empty = new Ordered(Seq.empty)

  def create(args: Parameter*): ParameterList = {
    new UnOrdered(args)
  }

  def id() = {
    new UnOrdered(Seq(new IdParameter()))
  }

  def address() = {
    new UnOrdered(Seq(new AddressParameter()))
  }

  def int(name: String, shortName: String) = {
    new UnOrdered(Seq(new IntParameter(name, shortName)))
  }

  def str(name: String, shortName: String) = {
    new UnOrdered(Seq(new StringParameter(name, shortName)))
  }
}

class Ordered(params: Seq[Parameter]) extends ParameterList(params) {
  val indexes = (0 to params.length - 1).permutations.toSeq

  override protected def validate(list: List[String], i: Int): Boolean = {
    indexes.exists(index => validateCore(list, index, 0))
  }

  private def validateCore(list: List[String], index: Seq[Int], i: Int): Boolean = {
    i < params.length && (list match {
      case n :: v :: Nil => if (i == params.length - 1) params(index(i)).validate(n, v) else false
      case n :: v :: tail if params(index(i)).validate(n, v) => validateCore(tail, index, i + 1)
      case _ => false
    })
  }
}

class UnOrdered(params: Seq[Parameter]) extends ParameterList(params) {

  case class TrackItem(parameter: Parameter, var flag: Boolean = false) {
    def markThenValidate(n: String, v: String) = {
      if (!flag) {
        flag = true
        parameter.validate(n, v)
      } else {
        false
      }
    }
  }

  class Track(params: Seq[Parameter]) {
    val dic = params.map(p => p.shortName -> TrackItem(p, false)).toMap
    //val dic = params.flatMap(p => Seq(p.shortName -> p, p.name -> p)).map(p => p._1 -> TrackItem(p._2, false)).toMap
    val regex = """^-(.*)""".r
    var halt = false
    var cloneDic = dic

    def reset() = {
      dic.values.foreach(_.flag = false)
      this
    }

    //
    //    def finalResult() = {
    //      dic.values.forall(_.flag)
    //    }

    def validate(name: String, v: String): Boolean = {
      name match {
        case regex(n) => dic.get(n) match {
          case Some(item) =>
            // 判断是否为可跳过参数
            if(halt && item.parameter.halt) halt
            else{
              // 验证参数规则
              val validate = item.markThenValidate(n, v)
              // 定义可跳过参数值正确
              if(validate && item.parameter.halt) halt = true
              // 将克隆dic集合减少
                cloneDic = cloneDic.-(n)
              validate
            }
          case None => false
        }
        case _ => false
      }
    }
  }

  private val track = new Track(params)

  override protected def validate(list: List[String], i: Int): Boolean = {

    // 验证若是帮助参数，返回true
    if(list.size==0 && track.dic.size == 1) {
      var validate = false
      track.dic.keys.foreach { i =>
        val value = track.dic.get(i)
        if (value.get.parameter.halt && !value.get.parameter.replaceable) {
          validate = true
        }
      }
        validate
    }else if(Command.checkHelpParam(list)){
      true
    }else{
      // 重新赋值克隆dic值
      track.cloneDic = track.dic

      // 验证 参数值 是否合规
      val validateV = validateCore(list, track.reset)

      // 验证无 参数 是否合规
      val validateP = validateParam(track.cloneDic)

      track.halt = false
      if(validateV && validateP) true
      else false
    }

  }

  private def validateParam(cloneDic: Map[String, TrackItem]) : Boolean = {
    var validate = true

    // 循环剩余参数进行判断
    breakable{cloneDic.values.foreach{i =>

      // 设置参数值为空
      i.parameter.validate("","",true)
        // 如果有不能终止的参数，返回false
        if(!i.parameter.halt){
          validate = false
          break()
        }
        // 判断为可替代项也返回false
        else if(!track.halt && i.parameter.replaceable){
          validate = false
          break()
        }
      }}
    validate
  }

  private def validateCore(list: List[String], track: Track): Boolean = {
    list match {
      case n :: v :: Nil => track.validate(n, v)
      case n :: v :: tail if track.validate(n, v) => validateCore(tail, track)
      case _ => false
    }
  }
}