package com.apex.cli


object APP {

  def main(args: Array[String]): Unit = {
    // 接受用户rpc请求地址
    val parmLen = args.length
    if (parmLen > 0) {
      if (parmLen == 2) {
        if (args(0).equals("-p") || args(0).equals("-P")) {
          val port = args(1)
          if (!verifyIp(port)) {
            System.out.println("Parameter error!")
            return
          }
        }
      } else {
        System.out.println("Parameter error!")
        return
      }
    }

    println("Welcome to CLI, type \"help\" for command list:")
    while (true) {
      // 获取用户输入信息，并执行
      Command.execute(Console.in.readLine()) match {
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
          sys.exit()
      }
    }
  }

  private def verifyIp(ipStr: String): Boolean = {
    var flag: Boolean = true
    if (ipStr != null && !(ipStr == "")) {
      if (ipStr.matches("(localhost\\:\\d{1,5})")) {
        RPC.rpcUrl = "http://" + ipStr + "/"
      }
      else {
        if (ipStr.matches("((\\d{1,3})\\.(\\d{1,3})\\.(\\d{1,3})\\.(\\d{1,3})\\:\\d{1,5})")) {
          RPC.rpcUrl = "http://" + ipStr + "/"
        }
        else {
          flag = false
        }
      }
    }
    else {
      flag = false
    }
    return flag
  }

}
