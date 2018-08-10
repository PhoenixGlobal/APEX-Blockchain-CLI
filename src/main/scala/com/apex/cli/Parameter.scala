package com.apex.cli

import play.api.libs.json.{JsNumber, JsObject, JsString, JsValue}

trait Parameter {
  val name: String
  val shortName: String

  def toJson(): JsValue

  def validate(n: String, v: String): Boolean

  protected def validateName(s: String): Boolean = {
    s.equals(s"-$shortName") || s.equals(s"-$name")
  }
}

class IntParameter(override val name: String, override val shortName: String) extends Parameter {
  private var value: Int = 0

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

class StringParameter(override val name: String, override val shortName: String) extends Parameter {
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

class IdParameter(override val name: String = "id", override val shortName: String = "id") extends Parameter {
  private var value: String = null
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
    //    s match {
    //      case regex(s) => {
    //        value = s
    //        true
    //      }
    //      case _ => false
    //    }
  }
}

class AddressParameter(override val name: String = "address", override val shortName: String = "address") extends Parameter {
  private var value: String = null

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

class AmountParameter(override val name: String = "amount", override val shortName: String = "amount") extends Parameter {
  private var value: BigDecimal = null

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

  protected def validate(list: List[String], i: Int): Boolean
}

object ParameterList {
  val empty = new AndList(Seq.empty)

  def and(args: Parameter*): ParameterList = {
    new AndList(args)
  }

  def or(args: Parameter*): ParameterList = {
    new OrList(args)
  }

  def id() = {
    new AndList(Seq(new IdParameter()))
  }

  def address() = {
    new AndList(Seq(new AddressParameter()))
  }

  def int(name: String, shortName: String) = {
    new AndList(Seq(new IntParameter(name, shortName)))
  }

  def str(name: String, shortName: String) = {
    new AndList(Seq(new StringParameter(name, shortName)))
  }
}

class AndList(params: Seq[Parameter]) extends ParameterList(params) {
  val indexes = (0 to params.length - 1).permutations.toSeq

  override protected def validate(list: List[String], i: Int): Boolean = {
    indexes.exists(index => validateCore(list, index, 0))
  }

  override def toString: String = {
    params.map(p => s"-${p.shortName}").mkString(",")
  }

  private def validateCore(list: List[String], index: Seq[Int], i: Int): Boolean = {
    i < params.length && (list match {
      case n :: v :: Nil => if(i == params.length - 1) params(index(i)).validate(n, v) else false
      case n :: v :: tail if params(index(i)).validate(n, v) => validateCore(tail, index, i + 1)
      case _ => false
    })
  }
}

class OrList(params: Seq[Parameter]) extends ParameterList(params) {
  override protected def validate(list: List[String], i: Int): Boolean = {
    list match {
      case n :: v :: Nil => params(i).validate(n, v)
      case n :: v :: tail if params(i).validate(n, v) => validate(tail, i + 1)
      case _ :: _ :: tail => validate(tail, i + 1)
      case _ => false
    }
  }

  override def toString: String = {
    s"-${params.map(_.shortName).mkString("|")}"
  }
}