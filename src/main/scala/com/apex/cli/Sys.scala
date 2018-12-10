package com.apex.cli

import java.io.{BufferedReader, InputStreamReader}

class SysCommand extends CompositeCommand {
  override val cmd: String = "sys"
  override val description: String = "Command Line Interface to the system, omit it and type the sub command directly is legal."
  override val composite: Boolean = true

  override val subCommands: Seq[Command] = Seq(
    new VersionC,
    new HelpC,
    new VerC,
    new ExitC,
    new ClearC
  )
}

class HelpC extends Command {
  override val cmd: String = "help"
  override val description: String = "help"
  override val sys: Boolean = true

  override def execute(params: List[String]): Result = {

    Help(Command.helpMessage("APEX NETWORK\n", Command.all, true))
  }
}

class VersionC  extends Command {
  override val cmd = "version"
  override val description = "Version information"
  override def execute(params: List[String]): Result = {Success("1")}
}

class VerC extends VersionC {
  override val cmd = "ver"
  override val sys: Boolean = true
}

class ExitC extends Command {
  override val cmd = "exit"
  override val description = "exit"
  override val sys: Boolean = true

  override def execute(params: List[String]): Result = new Quit
}

class ClearC extends Command {
  override val cmd = "clear"
  override val description = "Clear characters on screen"
  override val sys: Boolean = true

  override def execute(params: List[String]): Result = {


/*
    val console=System.console();

    if(console!=null)
    {
      System.out.println("input data");
      var pwds=console.readPassword();
      System.out.println("pwds="+pwds);

      pwds=console.readPassword("hello password %s", "test");
      System.out.println(pwds);

      console.writer().println("finish");
      console.flush();


    }else {
      System.out.println("console==null");
    }
*/


    /*try {
      val reader = new ConsoleReader()
      val mask = '0';
      val s =  reader.readLine("s>", mask)
      println(s)
    }catch{
      case e: Exception => Error(e)
    }*/

    /* val reader = LineReaderBuilder.builder().build()
     val prompt = "aaaa"
     while (true) {
       var line = ""
       try {
         line = reader.readLine(prompt, '*')
         println(line)
       } catch{
         case e: UserInterruptException => Error(e)
         case e: EndOfFileException => Error(e)

       }
     }*/

    Success("clear success\n")
  }
}
