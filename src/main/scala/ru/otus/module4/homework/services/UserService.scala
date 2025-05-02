package ru.otus.module4.homework.services

import zio._
import io.getquill.context.ZioJdbc.QIO
import ru.otus.module4.homework.dao.entity.{Role, RoleCode, User}
import ru.otus.module4.homework.dao.repository.UserRepository
import ru.otus.module4.phoneBook.db
import ru.otus.module4.homework.dao.entity.UserId
import org.scalafmt.config.Docstrings.BlankFirstLine.yes

trait UserService{
    def listUsers(): QIO[List[User]]
    def listUsersDTO(): QIO[List[UserDTO]]
    def addUserWithRole(user: User, roleCode: RoleCode): QIO[UserDTO]
    def listUsersWithRole(roleCode: RoleCode): QIO[List[UserDTO]]
}
class Impl(userRepo: UserRepository) extends UserService {
    val dc = db.Ctx

    def listUsers(): QIO[List[User]] = userRepo.list()

    def listUsersDTO(): QIO[List[UserDTO]] = for {
        users <- listUsers()
        usersDTO <- ZIO.foreach(users){ user =>
                        userRepo.userRoles(user.typedId).map { roles =>
                            UserDTO(user, roles.toSet)
                        }
                    }
    } yield usersDTO

    def addUserWithRole(user: User, roleCode: RoleCode): QIO[UserDTO] = for {
        _ <- userRepo.insertRoleToUser(roleCode, user.typedId)
        roles <- userRepo.userRoles(user.typedId)
    } yield UserDTO(user, roles.toSet)

    def listUsersWithRole(roleCode: RoleCode): QIO[List[UserDTO]] = for {
        users <- userRepo.listUsersWithRole(roleCode)
        usersDTO <- ZIO.foreach(users){ user =>
                        userRepo.userRoles(user.typedId).map { roles =>
                            UserDTO(user, roles.toSet)
                        }
                    }
    } yield usersDTO
}
object UserService{
    val layer: ZLayer[UserRepository, Nothing, UserService] = ZLayer.fromZIO(
        for {
            repo <- ZIO.service[UserRepository]
        } yield Impl(repo)
    )
}

case class UserDTO(user: User, roles: Set[Role])