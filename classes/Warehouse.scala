package main.scala.classes

import main.scala.classes.Database._
import org.squeryl.PrimitiveTypeMode._
import org.squeryl.dsl.OneToMany
import main.scala.classes.LocationType._
import org.squeryl.Table
import ActiveProductMethod._
import scala.collection.mutable.ListBuffer
import java.util.Date
import ProductMethod._
import LocationMethod._

//Warehouse Class
class Warehouse(val locationName:String, var locationId: Long) extends Basic{
  lazy val products: OneToMany[Product] = warehouseToProduct.left(this)
  def this() = this("", 0)
}

object WarehouseMethod {
  //AddWarehouse
  def newWarehouse(locationName: String): Long= inTransaction {
    val loc = newLocation(locationName)
    val war = setLocAsWarehouse(loc)
    return war
  }

  def getWarehouseLocationId(warehouseId :Long):Long ={
    val newwarehouse = warehouseTable.where(w=>w.id === warehouseId).single
    return newwarehouse.locationId
  }

  import ActiveProductMethod._
  def productStockAtLocation (productName: String, locationId: Long): Long = inTransaction{
    val products = getVendingProduct(productName, locationId, getLocationType(locationId))
    var count = 0
    for (i <- products){
      count += 1
    }
    return count
  }


  //RestockWH
  def RestockWarehouse(locationId: Long, productName: String, expiryDate: Date, qty: Long): List[Long] = inTransaction{
    val w = warehouseTable.where(w => w.id === locationId).single
    //location : Warehouse
    //item : Product
    //{location |-> item} :< ActiveProd
    if (!w.isInstanceOf[Warehouse]){
      println("Error: Location not a warehouse")
      return null
    }
    if (!activeProductExists(productName)){
      println("Error: "+productName + " does not exist yet. Add a new product?")
      return null
    }
    val products = new ListBuffer[Long]
    var i: Int = qty.toInt
    while(i > 0){
      val p = newProduct(productName,locationId,LocationType.warehouse, expiryDate)
      products += p
      i = i - 1
    }
    return products.toList
  }

  def printAll() {
    for (l <- from(warehouseTable)(a => select(a))) {
      println(l.id + " " + l.locationName + " " + l.locationId)
    }
  }

}