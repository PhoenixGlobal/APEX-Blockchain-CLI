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
  /*  val sys: Boolean = false*/
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

  def execute(command: String): Result = {
    if (!command.trim.isEmpty) {
      val list = command.trim.split("""\s+""").toList
      execCommand(list, all)
    } else {
      NoInput()
    }
  }

  def execCommand(list: List[String], all: Map[String, Seq[Command]]): Result = {
    list match {
      case cmd :: tail if all.contains(cmd) =>
        all(cmd).find(_.validate(tail)) match {
          case Some(command) =>
            if (checkHelpParam(tail) && !command.composite) Help(helpPMessage(command))
            else if (!command.composite && tail.size > 0 &&
              (command.paramList.params.isEmpty || command.paramList.params == null))
              InvalidParams(tail.mkString(" "))
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

      val column = s"${paddingTail("name", 15)} description"

      val content = command.paramList.params.map(p => {
        s"${paddingTail("-" + p.shortName, 15)} ${p.description}"
      }).mkString("\n")
      message = s"$title\n$column\n$content"
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

trait CompositeCommand extends Command {
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

