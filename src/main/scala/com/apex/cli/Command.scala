package com.apex.cli

import play.api.libs.json.Json

trait Command {
  val cmd: String
  val description: String
  val paramList: ParameterList = ParameterList.empty

  def execute(params: List[String]): Boolean = {
    if (paramList.validate(params)) {
      callRPC()
      true
    } else {
      false
    }
  }

  protected def callRPC(): Unit = {
    try {
      val result = RPC.post(cmd, paramList.toJson)
      println(Json prettyPrint result)
    } catch {
      case e: Throwable => e.printStackTrace
    }
  }
}

class ListBlock extends Command {
  override val cmd = "listblock"
  override val description = "list block"
}

class GetBlockById extends Command {
  override val cmd = "getblock"
  override val description = "get block by id"
  override val paramList: ParameterList = ParameterList.id
}

class GetBlockByHeight extends Command {
  override val cmd = "getblock"
  override val description = "get block by height"
  override val paramList: ParameterList = ParameterList.int("height", "h")
}

class GetBlockCount extends Command {
  override val cmd = "getblockcount"
  override val description = "get block count"
  override val paramList: ParameterList = ParameterList.empty
}

class ProduceBlock extends Command {
  override val cmd = "produceblock"
  override val description = "produce block"
  override val paramList: ParameterList = ParameterList.empty
}

class GetTransaction extends Command {
  override val cmd = "gettx"
  override val description = "get transaction"
  override val paramList: ParameterList = ParameterList.id
}

class Transfer extends Command {
  override val cmd = "send"
  override val description = "get transaction"
  override val paramList: ParameterList = ParameterList.and(
    new AddressParameter(),
    new IdParameter("assetId", "assetId"),
    new AmountParameter()
  )
}

class Help extends Command {
  override val cmd: String = "help"
  override val description: String = "help"

  private var helpMessage: String = null

  override protected def callRPC(): Unit = {
    def paddingTail(str: String, padding: Int): String = {
      str.formatted(s"%-${padding}s")
    }

    if (helpMessage == null) {
      val title = "APEX NETWORK\n"
      val column = s"${paddingTail("name", 15)} ${paddingTail("parameter", 20)} description"
      val content = Command.all.flatMap(
        p => p._2.map(c => {
          val cmd = if (c == p._2(0)) c.cmd else ""
          val params = s"[${c.paramList}]"
          s"${paddingTail(cmd, 15)} ${paddingTail(params, 20)} ${c.description}"
        })).mkString("\n")
      helpMessage = s"$title\n$column\n$content"
    }
    println(helpMessage)
  }
}

class Quit extends Command {
  override val cmd = "quit"
  override val description = "quit"

  override def callRPC(): Unit = {}
}

class Exit extends Command {
  override val cmd = "exit"
  override val description = "exit"

  override def callRPC(): Unit = {}
}

object Command {
  def execute(command: String): Boolean = {
    if (!command.trim.isEmpty) {
      val list = command.trim.split(" ").toList
      list match {
        case cmd :: tail if all.contains(cmd) =>
          if (!all.get(cmd).get.exists(_.execute(tail))) {
            println(s"invalid parameter(s) ${tail.mkString(" ")}")
          }
          true
        case cmd :: tail if sys.contains(cmd) =>
          sys.get(cmd).get.foreach(_.execute(tail))
          true
        case cmd :: _ if quit.contains(cmd) =>
          false
        case _ => println(s"unknown command: $command")
          true
      }
    } else {
      true
    }
  }

  val sys = Seq(
    new Help
  ).groupBy(_.cmd)

  val all = Seq(
    new ListBlock,
    new GetBlockById,
    new GetBlockByHeight,
    new GetBlockCount,
    new ProduceBlock,
    new Transfer,
    new GetTransaction
  ).groupBy(_.cmd)

  val quit = Seq(
    new Quit,
    new Exit
  ).groupBy(_.cmd)
}


