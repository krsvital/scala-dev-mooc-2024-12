package ru.otus.module4.phoneBook

import com.zaxxer.hikari.HikariDataSource
import io.getquill._
import io.getquill.context.ZioJdbc
import io.getquill.util.LoadConfig
import liquibase.Liquibase
import liquibase.database.jvm.JdbcConnection
import liquibase.resource.{ClassLoaderResourceAccessor, CompositeResourceAccessor, FileSystemResourceAccessor}
import ru.otus.module4.homework.configuration.Configuration
import zio.{Scope, ULayer, ZIO, ZLayer}

import javax.sql.DataSource


package object db {


  object Ctx extends PostgresZioJdbcContext(NamingStrategy(Escape, Literal))

  def hikariDS: HikariDataSource = new JdbcContextConfig(LoadConfig("db")).dataSource

  val zioDS: ZLayer[Any, Throwable, DataSource] = ZLayer.succeed(hikariDS)

  object LiquibaseService {

    trait LiquibaseService {
      def performMigration: ZIO[Liquibase, Throwable, Unit]
    }

    class Impl extends LiquibaseService {

      override def performMigration: ZIO[Liquibase, Throwable, Unit] = ZIO.serviceWith[Liquibase](_.update("dev"))
    }
     
    def mkLiquibase(): ZIO[DataSource, Throwable, Liquibase] = ZIO.scoped(for {
      config <- Configuration.config
      ds <- ZIO.service[DataSource]
      fileAccessor <-  ZIO.attempt(new FileSystemResourceAccessor())
      classLoader <- ZIO.attempt(classOf[LiquibaseService].getClassLoader)
      classLoaderAccessor <- ZIO.attempt(new ClassLoaderResourceAccessor(classLoader))
      fileOpener <- ZIO.attempt(new CompositeResourceAccessor(fileAccessor, classLoaderAccessor))
      jdbcConn <- ZIO.acquireRelease(ZIO.attempt(new JdbcConnection(ds.getConnection())))(r => ZIO.attempt(r.close()).orDie)
      liqui <- ZIO.attempt(new Liquibase(config.liquibase.changeLog, fileOpener, jdbcConn))
    } yield liqui)


    def performMigration: ZIO[LiquibaseService & Liquibase, Throwable, Unit]  = ZIO.serviceWithZIO[LiquibaseService](_.performMigration)

    val liquibase: ZLayer[DataSource, Throwable, Liquibase] = ZLayer.fromZIO(mkLiquibase())

    val live: ULayer[LiquibaseService] = ZLayer.succeed(new Impl)

  }
}
