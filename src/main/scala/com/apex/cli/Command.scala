package com.apex.cli

/*
 * Copyright  2018 APEX Technologies.Co.Ltd. All rights reserved.
 *
 * FileName: Command.scala
 *
 * @author: ruixiao.xiao@chinapex.com: 18-8-10 下午1:55@version: 1.0
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
  val sys: Boolean = false

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
            if(checkHelpParam(tail)) Help(command.description)
            else command.execute(tail)
          case None =>
            InvalidParams(tail.mkString(" "))
        }
      case cmd :: _ => UnKnown(cmd)
      case _ =>
        NoInput()
    }
  }

  def helpMessage(all: Map[String, Seq[Command]]): String = {

    var message: String = null
    def paddingTail(str: String, padding: Int): String = {
      str.formatted(s"%-${padding}s")
    }

    if (message == null) {
      val title = "APEX NETWORK\n"

      val column = s"${paddingTail("name", 15)} ${paddingTail("parameter", 15)} description"

      val content = all.flatMap(
        p => p._2.filterNot(_.sys).map(c => {
          val cmd = if (c == p._2(0)) c.cmd else ""

          val params = s"[${c.paramList}]"
          s"${paddingTail(cmd, 15)} ${paddingTail(params, 15)} ${c.description}"

        })).mkString("\n")
      message = s"$title\n$column\n$content"
    }

    message;
  }

  def checkHelpParam(params: List[String]):Boolean={
    if(params.isEmpty || params.size>1 )false
    else if(!params(0).equals("-h")) false
    else true
  }

  val all = Seq(
    new WalletCommand,
    new AccountCommand,
    new SysCommand,
    new ChainCommand,
    new AssetCommand,
    new HelpC,
    new VerC,
    new ExitC,
    new VersionC,
    new ClearC,
    new StatusCommand,
    new BlockCommand,
    new SendCommand,
    new CirculateCommand
  ).groupBy(_.cmd)
}

trait CompositeCommand extends Command {
  val subCommands: Seq[Command]

  override def execute(params: List[String]): Result = {

    if(params.size == 0){

      Help(Command.helpMessage(subCommands.groupBy(_.cmd)))
    }else if(params(0).startsWith("-")){

      if(Command.checkHelpParam(params)){
        Help(description)
      }else InvalidParams(params.mkString(" "))

    }else{
      Command.execCommand(params, subCommands.groupBy(_.cmd))
    }

    /*val test = subCommands.groupBy(_.cmd)
    println(test)
    val result = Command.execCommand(params, test)
    result match {
      case NoInput() => println(subCommands.map(_.cmd).mkString("\n"))
        case _ =>
    }
    result*/
  }
}
