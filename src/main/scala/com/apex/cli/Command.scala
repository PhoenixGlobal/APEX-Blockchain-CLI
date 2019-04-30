package com.apex.cli

/*
 * Copyright  2018 APEX Technologies.Co.Ltd. All rights reserved.
 *
 * FileName: Command.scala
 *
 * @author: whitney.wei@chinapex.com: 18-11-10 @version: 1.0
 */

import play.api.libs.json.Json


trait Result

case class Error(e: Throwable) extends Result

case class Success(data: String) extends Result

case class InvalidParams(input: String) extends Result

case class UnKnown(cmd: String) extends Result

case class Help(message: String) extends Result

case class NoInput() extends Result

case class Quit() extends Result

trait Command {
  val cmd: String
  val description: String
  val paramList: ParameterList = ParameterList.empty
  // 是否为复合命令
  val composite: Boolean = false

  def validate(params: List[String]): Boolean = {
    paramList.validate(params)
  }

  def execute(params: List[String]): Result = {
    try {
      Success(callRPC)
    } catch {
      case e: Throwable => Error(e)
    }
  }

  protected def callRPC(): String = {
    val result = RPC.post(cmd, paramList.toJson)
    Json prettyPrint result
  }
}

object Command {
  var reg ="""\(.+\)""".r

  def execute(command: String): Result = {
    ParameterList.setNull()
    // 判断是否为空
    if (!command.trim.isEmpty) {
      val list = preProcess(command.trim).split("""\s+""").toList
      execCommand(list, all)
    } else {
      NoInput()
    }
  }

  /**
    * @param command
    * @return remove space in (), eg: (ab  cd) => (abcd)
    */
  def preProcess(command: String): String = {
    val mat = reg.findFirstIn(command)
    if (mat.isDefined) {
      val matBefore = mat.get
      val matAfter = matBefore.replaceAll("\\s+", "")
      command.replace(matBefore, matAfter)
    } else
      command
  }

  def execCommand(list: List[String], all: Map[String, Seq[Command]]): Result = {
    list match {
      // 将集合分为1：其它，判断命令是否存在
      case cmd :: tail if all.contains(cmd) =>
        // 验证参数信息
        all(cmd).find(_.validate(tail)) match {
          case Some(command) =>
            // 如果是帮助参数，则直接返回
            if (checkHelpParam(tail) && !command.composite) Help(helpPMessage(command))
            // 如果不是复合命令并且命令参数为空，却输入了参数信息，则提示参数错误
            else if (!command.composite && tail.size > 0 &&
              (command.paramList.params.isEmpty || command.paramList.params == null))
              InvalidParams(tail.mkString(" "))
            // 调用真正的执行方法
            else command.execute(tail)
          case None =>
            InvalidParams(tail.mkString(" "))
        }
      case cmd :: _ => UnKnown(cmd)
      case _ =>
        NoInput()
    }
  }

  def helpMessage(titleMsg: String, all: Map[String, Seq[Command]], compositeMsg: Boolean = false): String = {

    var message: String = null

    def paddingTail(str: String, padding: Int): String = {
      str.formatted(s"%-${padding}s")
    }

    if (message == null) {
      val title = titleMsg
      val column = s"${paddingTail("name", 15)} description"

      val content = all.flatMap(
        p => p._2.filter((!compositeMsg || _.composite)).map(c => {
          val cmd = if (c == p._2(0)) c.cmd else ""

          /*val params = s"[${c.paramList}]"*/
          s"${paddingTail(cmd, 15)} ${c.description}"

        })).mkString("\n")
      message = s"$title\n$column\n$content"
    }

    message
  }


  def helpPMessage(command: Command): String = {

    var message: String = null

    def paddingTail(str: String, padding: Int): String = {
      str.formatted(s"%-${padding}s")
    }

    if (message == null) {
      val title = command.description
      if (command.paramList.params.size > 0) {
        val column = s"${paddingTail("name", 15)} description"

        val content = command.paramList.params.map(p => {
          s"${paddingTail("-" + p.shortName, 15)} ${p.description}"
        }).mkString("\n")
        message = s"$title\n$column\n$content"
      } else {
        message = title
      }
    }

    message;
  }


  def checkHelpParam(params: List[String]): Boolean = {
    if (params.isEmpty || params.size > 1) false
    else if (!params(0).equals("-h")) false
    else true
  }

  val all = Seq(
    new WalletCommand,
    new AccountCommand,
    new SysCommand,
    new ChainCommand,
    new AssetCommand,
    new ContractCommand,
    new ProducerCommand,

    new HelpC,
    new VerC,
    new ExitC,
    new VersionC,
    new ClearC,

    new StatusCommand,
    new BlockCommand,

    new SendCommand
  ).groupBy(_.cmd)
}

// 复合命令的执行方法
trait CompositeCommand extends Command {
  // 子命令行
  val subCommands: Seq[Command]

  override def execute(params: List[String]): Result = {

    if (params.isEmpty || Command.checkHelpParam(params)) {
      Help(Command.helpMessage(description, subCommands.groupBy(_.cmd)))
    } else {
      Command.execCommand(params, subCommands.groupBy(_.cmd))
      // 排除类似 sys clear 二级命令
      /*Command.execCommand(params, subCommands.filterNot(_.sys).groupBy(_.cmd))*/
    }
  }
}

