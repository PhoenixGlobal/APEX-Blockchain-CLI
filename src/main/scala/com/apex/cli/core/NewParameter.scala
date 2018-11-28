/*
 * Copyright  2018 APEX Technologies.Co.Ltd. All rights reserved.
 *
 * FileName: Parameter.scala
 *
 * @author: ruixiao.xiao@chinapex.com: 18-8-10 下午1:55@version: 1.0
 */

package com.apex.cli.core

import play.api.libs.json.{JsNumber, JsObject, JsString, JsValue}

trait NewParameter {
  val name: String
  val shortName: String
  val halt : Boolean = false


  def toJson(): JsValue

  def validate(n: String, v: String): Boolean

  protected def validateName(s: String): Boolean = {
    s.trim.equals(shortName) || s.trim.equals(name)
  }
}

class NewIntParameter(override val name: String, override val shortName: String, override val halt: Boolean = false) extends NewParameter {
  var value: Int = 0

  override def toJson: JsValue = JsNumber(value)

  override def validate(n: String, v: String): Boolean = {
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

class NewStringParameter(override val name: String, override val shortName: String) extends NewParameter {
  private var value: String = null

  override def toJson: JsValue = JsString(value)

  override def validate(n: String, v: String): Boolean = {
    validateName(n) && setValue(v)
  }

  private def setValue(s: String): Boolean = {
    value = s
    true
  }
}

class NewIdParameter(override val name: String = "id", override val shortName: String = "id") extends NewParameter {
  var value: String = null
  //  private val regex = """[0-9a-fA-F]32""".r

  override def toJson: JsValue = JsString(value)

  override def validate(n: String, v: String): Boolean = {
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

class NewAddressParameter(override val name: String = "address", override val shortName: String = "address", override val halt: Boolean = false) extends NewParameter {
  var value: String = null

  override def toJson: JsValue = JsString(value)

  override def validate(n: String, v: String): Boolean = {
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

class HelpParameter(override val name: String = "help", override val shortName: String = "h") extends NewParameter {
  var value: String = null

  override def toJson: JsValue = JsString(value)

  override def validate(n: String, v: String): Boolean = {
    validateName(n) && setValue(v)
  }

  private def setValue(s: String): Boolean = {
    false
  }
}

class NicknameParameter(override val name: String, override val shortName: String, override val halt: Boolean = false) extends NewParameter {
  var value: String = null

  private val regex = """^(?![0-9]+$)(?![a-zA-Z]+$)[0-9A-Za-z]{1,12}$""".r

  override def toJson: JsValue = JsString(value)

  override def validate(n: String, v: String): Boolean = {
    validateName(n) && setValue(v)
  }

  private def setValue(s: String): Boolean = {
    if(regex.pattern.matcher(s).matches()){
      value = s
      true
    }else false
  }
}

class PasswordParameter(override val name: String = "password", override val shortName: String = "p") extends NewParameter {
  var value: String = null
  private val regex = """^(?![0-9]+$)(?![a-zA-Z]+$)[0-9A-Za-z]{8,}$""".r

  override def toJson: JsValue = JsString(value)

  override def validate(n: String, v: String): Boolean = {
    validateName(n) && setValue(v)
  }

  private def setValue(s: String): Boolean = {
    if(regex.pattern.matcher(s).matches()){
      value = s
      true
    }else false
  }
}

class NewPrivKeyParameter(override val name: String = "privkey", override val shortName: String = "privkey") extends NewParameter {
  var value: String = null

  override def toJson: JsValue = JsString(value)

  override def validate(n: String, v: String): Boolean = {
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


class NewAmountParameter(override val name: String = "amount", override val shortName: String = "amount") extends NewParameter {
  var value: BigDecimal = null

  override def toJson: JsValue = JsString(value.toString)

  override def validate(n: String, v: String): Boolean = {
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

abstract class NewParameterList(val params: Seq[NewParameter]) {
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

object NewParameterList {
  val empty = new NewOrdered(Seq.empty)

  def create(args: NewParameter*): NewParameterList = {
    new NewUnOrdered(args)
  }

  def id() = {
    new NewUnOrdered(Seq(new NewIdParameter()))
  }

  def address() = {
    new NewUnOrdered(Seq(new NewAddressParameter()))
  }

  def int(name: String, shortName: String) = {
    new NewUnOrdered(Seq(new NewIntParameter(name, shortName)))
  }

  def str(name: String, shortName: String) = {
    new NewUnOrdered(Seq(new NewStringParameter(name, shortName)))
  }
}

class NewOrdered(params: Seq[NewParameter]) extends NewParameterList(params) {
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

class NewUnOrdered(params: Seq[NewParameter]) extends NewParameterList(params) {

  case class TrackItem(parameter: NewParameter, var flag: Boolean = false) {
    def markThenValidate(n: String, v: String) = {
      if (!flag) {
        flag = true
        parameter.validate(n, v)
      } else {
        false
      }
    }
  }

  class NewTrack(params: Seq[NewParameter]) {
    val dic = params.map(p => p.shortName -> TrackItem(p, false)).toMap
    val regex = """^-(.*)""".r
    var halt = false
    var cloneDic = dic

    def reset() = {
      dic.values.foreach(_.flag = false)
      this
    }

    def validate(name: String, v: String): Boolean = {
      name match {
        case regex(n) =>
          dic.get(n) match {
          case Some(item) =>
            // 判断是否为可跳过参数
            if(halt && item.parameter.halt) return halt
            // 验证参数规则
            val validate = item.markThenValidate(n, v)
            // 定义可跳过参数值正确
            if(validate && item.parameter.halt) halt = true
            // 将克隆dic集合减少
            cloneDic = cloneDic.-(n)
            validate
          case None => false
        }
        case _ => false
      }
    }
  }

  private val track = new NewTrack(params)

  override protected def validate(list: List[String], i: Int): Boolean = {

    // 验证若是帮助参数，返回true
    if(NewCommand.checkHelpParam(list)) return true

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

  private def validateParam(cloneDic: Map[String, TrackItem]) : Boolean = {
    // 循环剩余参数进行判断
    cloneDic.values.foreach{i =>
      if(!i.parameter.halt)  return false
    }
    true
  }

  private def validateCore(list: List[String], track: NewTrack): Boolean = {

    var result = false
    list match {
      case n :: v :: Nil => result = track.validate(n, v)
      case n :: v :: tail if track.validate(n, v) => result = validateCore(tail, track)
      case _ => result = false
    }
    result
  }
}