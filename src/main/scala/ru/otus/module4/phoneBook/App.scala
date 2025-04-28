package ru.otus.module4.phoneBook

import liquibase.Liquibase
import ru.otus.module4.phoneBook.api.PhoneBookAPI
import ru.otus.module4.phoneBook.dao.repositories.{AddressRepository, PhoneRecordRepository}
import ru.otus.module4.phoneBook.db.LiquibaseService
import ru.otus.module4.phoneBook.services.PhoneBookService
import zio.{Random, ZLayer}

import javax.sql.DataSource

object App {


    val httpApp = PhoneBookAPI.api

    val layer = ZLayer.makeSome[Random, PhoneBookService with DataSource with LiquibaseService.LiquibaseService with Liquibase](
        PhoneBookService.live, PhoneRecordRepository.live, AddressRepository.live,
        LiquibaseService.liquibase, LiquibaseService.live, db.zioDS)

//    val server = (LiquibaseService.performMigration *>
//    zhttp.service.Server.start(8080, httpApp))
//    .provideSomeLayer(layer)
}
