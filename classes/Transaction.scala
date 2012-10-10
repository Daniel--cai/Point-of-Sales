package main.scala.classes
import org.squeryl._
import org.squeryl.PrimitiveTypeMode._
import Database._
import ProductMethod._

class Transaction (val user: Long, val member: Long, var active: Boolean) extends Basic {
  def this(user: Long, member:Long) = this(user,member, true)
}
  //transaction to product
class Purchase(val transactionId: Long, val productId: Long){
  def this() = this(0, 0)
}

object TransactionMethod{
  def newTransaction (userId: Long, memberId: Long) : Long = inTransaction{
    val newtransaction = new Transaction(userId, memberId)
    transactionTable.insert(newtransaction)
    return newtransaction.id
  }

  //VoidCheckout
  def voidTransaction(id : Long)= inTransaction {
    //product : dom(Trolley)
    //Trolley := InitTrolley
    purchaseTable.deleteWhere(p => p.transactionId === id)
    transactionTable.deleteWhere((p => p.id === id))
  }
  //CheckOut
  def checkOut(transactionId: Long)= inTransaction{
    update(transactionTable)(t=>
      where(t.id === transactionId)
        set(t.active := false) )
  }
  //CheckOut
  def calculateTotal(transactionId: Long) :BigDecimal = inTransaction{
    var total: BigDecimal = 0
    //Trolley(products) > 0
    for (pur <- from(purchaseTable)(pur => where(pur.transactionId === transactionId)select(pur))) {
      val p = activeproductTable.where(ap => ap.productName === getProductName(pur.productId)).single
      //cost = Price(products) * numItems
      total = total + p.price
    }
    //AmountDue :=AmountDue + cost
    return total
  }
  //@products : dom(Trolley)
  def isProductInTransaction(transactionId: Long, productId: Long) :Boolean = inTransaction {
    if (purchaseTable.exists(p => p.productId == productId))
      return true
    return false
  }

  def transactionExists(transactionId: Long) :Boolean = inTransaction {
    if (transactionTable.exists(t => t.id == transactionId))
      return true
    return false
  }

  def getProductsInTransaction(transactionId: Long): List[Long] = inTransaction {
    val p = from(purchaseTable)(p=> where(p.transactionId === transactionId) select(p.productId))
    return p.toList
  }

  def getTransactionUser(transactionId: Long) :Long = inTransaction {
    val u = transactionTable.where(t => t.id === transactionId).single
    return u.user
  }

  def addProductToTransaction(transactionId: Long, productId: Long)= inTransaction{
    purchaseTable.insert(new Purchase(transactionId: Long, productId))
    setProductStatus(productId, ProductStatus.scanned)
  }

  // RemProdTrolley
  def removeProductFromTransaction(transactionId: Long, productId: Long)= inTransaction{
    purchaseTable.deleteWhere(p=>
      p.transactionId === transactionId and p.productId === productId )
    setProductStatus(productId, ProductStatus.vending)
  }

  import ProductStatus._
  def setProductStatus(productId: Long, status: ProductStatus)= inTransaction{
    update(productTable)(p=>
    where(p.id === productId)
    set(p.status := status))
  }

  //AddProdTrolley
  import ActiveProductMethod._
  def addProductQtyToTransaction(transactionId: Long, productName: String, qty: Long, locationId: Long)= inTransaction{
    //item : ran(ActiveProd)



    val products = getVendingProduct(productName, locationId)
    println("location : "+locationId + " length "+ products.length)
    if (products.length >= qty ){
      var i: Int = (qty.toInt) - 1
      while(i >= 0){
        //Trolley(item) := amount
        addProductToTransaction(transactionId, products(i))
        i = i - 1
      }
    } else {
      println("Not enough stock")
    }
  }
  import ProductMethod._
  def removeProductQtyFromTransaction(transactionId: Long, productName: String, qty: Long)= inTransaction{
    val products = from(purchaseTable)(p=>
                    where(p.transactionId === transactionId
                    and getProductName(p.productId) === productName)
                    select(p.productId)).toList
    if (products.length >= qty){
      var i: Int = (qty.toInt) - 1
      while(i >= 0){
        removeProductFromTransaction(transactionId, products(i))
        i = i - 1
      }
    } else {
      println("Not enough stock")
    }
  }
}
