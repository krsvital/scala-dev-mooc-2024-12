package ru.otus.module4.phoneBook.dao.repositories

import io.getquill.context.ZioJdbc._
import ru.otus.module4.phoneBook.dao.entities.{Address, PhoneRecord}
import ru.otus.module4.phoneBook.db
import zio.ULayer

trait PhoneRecordRepository{
  def find(phone: String): QIO[Option[PhoneRecord]]
  def list(): QIO[List[PhoneRecord]]
  def insert(phoneRecord: PhoneRecord): QIO[Unit]
  def update(phoneRecord: PhoneRecord): QIO[Unit]
  def delete(id: String): QIO[Unit]
}

class PhoneRecordRepositoryImpl extends PhoneRecordRepository{
  val ctx = db.Ctx
  import ctx._

  val phoneRecordSchema = quote{
    querySchema[PhoneRecord]("""PhoneRecord""")
  }

  val addressSchema = quote{
    querySchema[Address]("""Address""")
  }

  // SELECT x1."id" AS id, x1."phone" AS phone, x1."fio" AS fio, x1."addressId" AS addressId 
  // FROM PhoneRecord x1 WHERE x1."phone" = ?
  def find(phone: String): QIO[Option[PhoneRecord]] =
    ctx.run(phoneRecordSchema.filter(_.phone == lift(phone)).sortBy(_.phone).take(1)).map(_.headOption)
  
  //SELECT x."id" AS id, x."phone" AS phone, x."fio" AS fio, 
  // x."addressId" AS addressId FROM PhoneRecord xblo
  def list(): QIO[List[PhoneRecord]] =
    ctx.run(phoneRecordSchema)


    //INSERT INTO PhoneRecord ("id","phone","fio","addressId") 
    // VALUES (?, ?, ?, ?)
  def insert(phoneRecord: PhoneRecord): QIO[Unit] =
    ctx.run(phoneRecordSchema.insertValue(lift(phoneRecord))).unit

  def insert(phoneRecords: List[PhoneRecord]): QIO[Unit] =
    ctx.run(liftQuery(phoneRecords).foreach(phr => phoneRecordSchema
    .insertValue(phr))).unit


    // UPDATE PhoneRecord AS x3 SET "id" = ?, "phone" = ?, "fio" = ?, 
    // "addressId" = ? WHERE x3."id" = ?

  def update(phoneRecord: PhoneRecord): QIO[Unit] = 
    ctx.run(phoneRecordSchema.filter(_.id == lift(phoneRecord.id))
    .updateValue(lift(phoneRecord))).unit


  def delete(id: String): QIO[Unit] =
    ctx.run(phoneRecordSchema.filter(_.id == lift(id)).delete).unit

  // implicit join
  // SELECT phoneRecord."id" AS id, phoneRecord."phone" AS phone, phoneRecord."fio" AS fio, 
  // phoneRecord."addressId" AS addressId, address."id" AS id, address."zipCode" AS zipCode, address."streetAddress" AS streetAddress 
  // FROM PhoneRecord phoneRecord, Address address WHERE address."id" = phoneRecord."addressId"
  ctx.run(
    for{
      phoneRecord <- phoneRecordSchema
      address <- addressSchema if(address.id == phoneRecord.addressId)
    } yield (phoneRecord, address)
  )

  // applicative

  ctx.run(
    phoneRecordSchema.join(addressSchema).on(_.addressId == _.id)
    .filter(v => v._2.zipCode == lift("230012"))
  )

  // flat join
  ctx.run(
    for{
      phoneRecord <- phoneRecordSchema
      address <- addressSchema.join(_.id == phoneRecord.addressId)
    } yield phoneRecord
  )
}

object PhoneRecordRepository {



  val live: ULayer[PhoneRecordRepository] = ???
}
