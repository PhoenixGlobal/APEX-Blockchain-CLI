package com.apex.test

import com.apex.cli.{Command, Success}
import org.junit.Test

import scala.io.Source

@Test
class SystemInfo extends PreparedResources{

  @Test
  def testGetBlock(): Unit ={
    Command.inputCommand = "chain block -height 1"

    val result = commandExecutor(Command.inputCommand)

    assert(result.isInstanceOf[Success])


  }
}

