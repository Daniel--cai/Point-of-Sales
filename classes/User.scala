package main.scala.classes

import main.scala.classes.Database._
import org.squeryl.PrimitiveTypeMode._
import UserType._
class User (var name: String, var userType: UserType, var active: Boolean) extends Basic {
  def this() = this("", UserType.admin, true)
}

object UserMethod {
  def newUser(name: String, userType: UserType):Long = inTransaction{
    val user = new User(name, userType, true)
    userTable.insert(user)
    return user.id
  }
  //delete user
  def deactivateUser(userId: Long)= inTransaction{
    update(userTable)(u =>
      where(u.id === userId)
        set (u.active := false))
  }
  /*
  def getTransactionUser() :String = {
    val u = transactionTable.where(t => t.id === this.id).single
    return u.user
  }
  */
  def getUserType(userId: Long):UserType = inTransaction{
    val u = userTable.where(u=> u.id === userId).single
    return u.userType
  }

  def getUserName (userId: Long): String = inTransaction{
    val u = userTable.where(u=> u.id === userId).single
    return u.name
  }
}