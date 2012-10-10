package main.scala.classes

import org.squeryl._
import org.squeryl.PrimitiveTypeMode._
import main.scala.classes.Database._

class Member (var name: String, var points: Long, var active: Boolean) extends Basic {
  def this(name: String) = this(name, 0, true)
}

object MemberMethod {
  //JoinLoyaltyProgram
  def newMember(name: String) :Long = inTransaction {
    //member : MEMBER \ Member
    if (memberTable.exists(m => m.name.matches(name))) return -1
    val newmember = new Member(name)
    memberTable.insert(newmember)
    return newmember.id;
  }

  //RemoveLoyaltyProgram
  def removeMember(memberId: Long)= inTransaction{
    //member : dom(MemberPoints)
    if (!memberTable.exists(m => m.id == memberId)) {
      update(memberTable)(m=>
      where(m.id === memberId)
      set(m.active := false))
    }
  }

  def getPoints(memberId: Long):Long = inTransaction {
    val u = memberTable.where(m => m.id === memberId).single
    return u.points
  }


  //MemberPointGain
  def addPoint(memberId: Long, points : Long)= inTransaction{
    //member:Member
    //memberExists(memberId)
    //points > 0

    update(memberTable)(m=>
      where (m.id === memberId)
        set(m.points := m.points plus points))
  }

  def removePoint(memberId: Long, points : Long)= inTransaction{
    update(memberTable)(m=>
    where (m.id === memberId)
    set(m.points := m.points minus points))
  }

  def memberExists(memberId: Long): Boolean = inTransaction{
    if (memberTable.exists(m => m.id == memberId))
      return true
    return false
  }
}
