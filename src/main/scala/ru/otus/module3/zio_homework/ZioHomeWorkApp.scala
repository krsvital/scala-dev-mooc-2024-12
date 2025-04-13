package ru.otus.module3.zio_homework

import zio.{ZLayer, Scope, ZIO, ZIOAppArgs, ZIOAppDefault, Console, Random}

object ZioHomeWorkApp extends ZIOAppDefault{
  override def run = runApp
} 