package main.scala.classes

import org.squeryl.PrimitiveTypeMode._
import java.util.Date
import main.scala.classes.Database._
import org.squeryl.dsl.ManyToOne
import ProductStatus._
import LocationType._

class Product (val productName: String, val locationId: Long, var locationType: LocationType, val expiryDate: Date, var status: ProductStatus) extends Basic{
  lazy val locations: ManyToOne[Location] = locationToProduct.right(this)
  lazy val warehouse: ManyToOne[Warehouse] = warehouseToProduct.right(this)


  def this(productName: String, expiryDate: Date) = this(productName,0,LocationType.location, expiryDate, ProductStatus.vending)
}


object ProductMethod {
  //AddProductLocation 2.1.3 - The system will keep track of stock qty
  //AddProductStore
  //AddProductBackstore
  def newProduct(productName: String, locationId: Long, locationType: LocationType, expiryDate : Date):Long= inTransaction{
    //item : Product
    //productLineExists(productName)
    //val l = locationTable.where(l=>l.id === locationId).single
    val newproduct = new Product(productName, expiryDate)
    if (locationType == LocationType.warehouse) {
      val data = warehouseTable.where(l => l.locationId === locationId).single
      data.products.associate(newproduct)
    }
    if (locationType == LocationType.location) {
      val data = locationTable.where(l => l.id === locationId).single
      data.products.associate(newproduct)
    }
    //Stock(location |-> item) = stock

    return newproduct.id
  }

  def printAll() = inTransaction{
    for(p <- {from (productTable) (t => select(t))}) {
      println(p.id+" "+p.productName+" "+p.locationType+" "+p.expiryDate)
    }
  }

  def productLineExists(productName: String):Boolean = inTransaction {
    return activeproductTable.exists(ap => ap.productName.matches(productName))
  }

  def getProductName(productId: Long):String = inTransaction{
    val p = productTable.where(p =>p.id === productId).single
    return p.productName
  }

  def getProductLocation(productId: Long): Long =inTransaction{
    val p = productTable.where(p =>p.id === productId).single
    return p.locationId
  }

  def moveProductLocation(productId:Long, locationId: Long)=inTransaction{
    val l = locationTable.where(l => l.id === locationId).single
    val p = productTable.where(p => p.id === productId).single
    println("Change locationId of product from "+p.locationId+" id: "+p.id)
    l.products.associate(p)
    val pro = productTable.where(p =>p.id === productId).single
    println("To "+pro.locationId)
  }
}


class ActiveProduct (val productName: String, var price: BigDecimal, var active: Boolean) extends Basic{
  def this(productName: String, price: BigDecimal) = this(productName, price, true)
}


object ActiveProductMethod {
  def getPrice(productName: String) : BigDecimal = inTransaction {
    val n = activeproductTable.where(ap => ap.productName === productName).single
    return n.price
  }
  //SetPrice
  def changePrice(productName: String, newPrice: BigDecimal) = inTransaction{
    //item : ran(ActiveProd)
    update(activeproductTable)(ap=>
      where(ap.productName === productName)
        set (ap.price := newPrice))
  }

  def productAtLocationExpired (productName: String, locationId: Long) : List[Product] = inTransaction{
    val products = from(locationTable, productTable)((l,p)=>
      where(l.id === locationId
        and l.id === p.locationId
        and p.productName === productName)
        //check expiry date gt getCalendar.getTime()
        select(p))
    return products.toList
  }


  //AddProductList //2.1.10 - The system will allow for entry of stock into the system
  def newActiveProduct (productName: String, price: BigDecimal) :Long = inTransaction {
    //item ∉ Product
    if (activeProductExists(productName)) return -1
    val ap = new ActiveProduct(productName, price)
    activeproductTable.insert(ap)
    return ap.id
  }

  def activeProductExists(productName: String): Boolean = inTransaction{
    return activeproductTable.exists(ap => ap.productName.matches(productName))
  }

  def getActiveProductId(productName: String) : Long =inTransaction{
    if (!activeProductExists(productName)) return -1
    val n = activeproductTable.where(p =>p.productName === productName).single
    return n.id
  }
  import ProductStatus._
  import LocationMethod._
  import LocationType._
  def getVendingProduct(productName: String, locationId: Long, locationType: LocationType):List[Long] = inTransaction{

    val products = locationType match{
      case LocationType.warehouse =>  from(warehouseTable, productTable)((l,p)=>
                                       where(l.id === locationId
                                       and l.id === p.locationId
                                       and p.productName === productName
                                       and p.status === ProductStatus.vending )
                                       select(p.id))
      //case LocationType.frontstore=> val product
      case LocationType.warehouse =>   from(locationTable, productTable)((l,p)=>
                                       where(l.id === locationId
                                       and l.id === p.locationId
                                        and p.productName === productName
                                        and p.status === ProductStatus.vending )
                                        select(p.id))
    }
    return products.toList
  }

}