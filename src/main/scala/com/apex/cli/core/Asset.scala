package com.apex.cli.core

class AssetCommand extends NewCompositeCommand {
  override val cmd: String = "asset"
  override val description: String = "Interface to operate your funds,  omit it and type the sub command directly is legal."

  override val subCommands: Seq[NewCommand] = Seq(

  )
}
