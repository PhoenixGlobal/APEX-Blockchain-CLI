package com.apex.test

import com.apex.cli.{Result, _}

class PreparedResources{
  def beforeEach: Unit ={
    RPC.rpcHandler = new FakeRPC()
  }

  beforeEach

  def commandExecutor(command: String): Result ={
    println("Welcome to CLI, type \"help\" for command list:")
    println(Command.inputCommand)
    println()

      val commandProcessResult = Command.execute(Command.inputCommand)
      // 获取用户输入信息，并执行
      commandProcessResult match {
        case UnKnown(cmd) =>
          println(s"unknown command: $cmd")
        case InvalidParams(input) => {
          if (!ParameterList.checkParamMsg.isEmpty)
            println(s"invalid parameters: " + ParameterList.checkParamMsg)
          else
            println(s"invalid parameters: $input")
        }
        case Success(result) =>
          println(result)
        case Help(message) =>
          println(message)
        case Error(e) =>
          println(e)
        case NoInput() => {}
        case Quit() =>
          println("exit system")
      }
      commandProcessResult

  }
}