package main.scala.classes

import main.scala.classes.Database._
import org.squeryl.PrimitiveTypeMode._
import java.util.{Calendar, Date}
import org.squeryl.Table

//one to many with product
class Reserve (val memberId: Long,  var pickupDate: Date) extends Basic {
}
class ReservedProduct(var reserveId: Long, var productId: Long){
  def this() = this(0,0)
}

object ReserveMethod {

  //ReserveProduct
  import TransactionMethod._
  def newReservation(memberId: Long, pickupDate: Date) :Long = inTransaction{
    //member : Member
    //if (!memberExists(memberId)) return -1
    val newreserve = new Reserve(memberId, pickupDate)
    reserveTable.insert(newreserve)
    return newreserve.id
  }
  //ReserveProduct
  def addProductToReservation(reserveId: Long, productId: Long)= inTransaction{
    //ReserveStock(store) := product
    //getLocationType(getProductLocation(productId)) == LocationType.frontstore
    reservedproductTable.insert(new ReservedProduct(reserveId: Long, productId))
  }

  def removeProductFromReservation(reserveId: Long, productId: Long)= inTransaction{
    reservedproductTable.deleteWhere(r => (r.reserveId === reserveId and r.productId === productId))
  }
  //CancelReserve
  def cancelReservation(reserveId: Long)= inTransaction{
    //ReservedStock := {}
    //ReserveNum(store |-> product = 0
    reservedproductTable.deleteWhere(r => (r.reserveId === reserveId))
    reserveTable.deleteWhere(r => (r.id === reserveId))

  }
  //BuyReserve
  def buyReservation(reserveId: Long, clerkId: Long): Long = inTransaction{
    //product = ReservedStock(store)
    val member =  reserveTable.where(r => r.id === reserveId).single
    val t = newTransaction(clerkId, member.memberId)
    for(r <- {from (reservedproductTable) (r => where(reserveId === r.reserveId) select(r))} ){
      addProductToTransaction(t, r.productId)
    }
    //MoneyBox := MoneyBox - cost
    //moved calculations to another method
    //returns transaction only for modification
    return t
  }

  def printReservationByMember(memberId : Long)= inTransaction{
    for(r <- {from (reservedproductTable) (r => where(memberId === r.reserveId) select(r))} ){
      println(r.productId)
    }
  }
}

