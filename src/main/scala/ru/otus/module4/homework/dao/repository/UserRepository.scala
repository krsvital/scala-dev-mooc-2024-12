package ru.otus.module4.homework.dao.repository

import zio.{ULayer, ZIO, ZLayer}
import io.getquill._
import io.getquill.context.ZioJdbc._
import ru.otus.module4.homework.dao.entity._
import ru.otus.module4.phoneBook.db

import java.sql.SQLException
import javax.sql.DataSource

trait UserRepository{
    def findUser(userId: UserId): QIO[Option[User]]
    def createUser(user: User): QIO[User]
    def createUsers(users: List[User]): QIO[List[User]]
    def updateUser(user: User): QIO[Unit]
    def deleteUser(user: User): QIO[Unit]
    def findByLastName(lastName: String): QIO[List[User]]
    def list(): QIO[List[User]]
    def userRoles(userId: UserId): QIO[List[Role]]
    def insertRoleToUser(roleCode: RoleCode, userId: UserId): QIO[Unit]
    def listUsersWithRole(roleCode: RoleCode): QIO[List[User]]
    def findRoleByCode(roleCode: RoleCode): QIO[Option[Role]]
}


class UserRepositoryImpl extends UserRepository {
    val dc = db.Ctx
    import dc._

    inline def userSchema = quote {
        querySchema[User]("""User""")
    }

    inline def userRole = quote {
        querySchema[Role]("""Role""")
    }

    inline def userToRole = quote {
        querySchema[UserToRole]("""UserToRole""")
    }

    override def findUser(userId: UserId): QIO[Option[User]] = dc.run(
        userSchema.filter { user =>
            user.id == lift(userId.id)
        }
    ).map(_.headOption)

    override def createUser(user: User): QIO[User] = dc.run(
        userSchema.insertValue(lift(user)).returningGenerated{ u => u }
    )

    override def createUsers(users: List[User]): QIO[List[User]] = dc.run(
        liftQuery(users).foreach { user =>
            userSchema.insertValue(user).returningGenerated{ u => u }
        }
    )

    override def updateUser(user: User): QIO[Unit] = dc.run(
        userSchema.filter { u =>
            u.id == lift(user.id)
        }.updateValue(lift(user))
    ).unit

    override def deleteUser(user: User): QIO[Unit] = dc.run(
        userSchema.filter { u =>
            u.id == lift(user.id)
        }.delete
    ).unit

    override def findByLastName(lastName: String): QIO[List[User]] = dc.run(
        userSchema.filter { u =>
            u.lastName == lift(lastName)
        }
    )

    override def list(): QIO[List[User]] = dc.run(userSchema)

    override def userRoles(userId: UserId): QIO[List[Role]] = dc.run(
        for {
            roles <- userRole
            user2role <- userToRole.join { u => u.userId == lift(userId.id) }
            _ <- userSchema.join { u => u.id == user2role.userId }
        } yield roles
    )

    override def insertRoleToUser(roleCode: RoleCode, userId: UserId): QIO[Unit] = dc.transaction(
        for {
            u2r <- dc.run(userRole.filter { role => role.code == lift(roleCode.code) }).map { _.headOption }.some.map { role => UserToRole(roleId = role.id, userId = userId.id) }.orElseFail(Throwable(s"role with code ${roleCode.code} not found"))
            _ <- dc.run(userToRole.insertValue(lift(u2r)))
        } yield ()
    ).mapError { e => SQLException(e.getMessage()) }

    override def listUsersWithRole(roleCode: RoleCode): QIO[List[User]] = dc.run(
        for {
            role <- userRole.filter { role => role.code == lift(roleCode.code) }
            user2role <- userToRole.join { u => u.roleId == role.id }
            users <- userSchema.join { u => u.id == user2role.userId }
        } yield users
    )

    override def findRoleByCode(roleCode: RoleCode): QIO[Option[Role]] = dc.run(
        userRole.filter { role => role.code == lift(roleCode.code) }
    ).map { _.headOption }
}

object UserRepository{
    val layer: ULayer[UserRepository] = ZLayer.succeed(new UserRepositoryImpl)
}