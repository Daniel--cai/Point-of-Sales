package main.scala.classes

//import classes.PoSWare
import main.scala.classes.Database._
import org.squeryl.PrimitiveTypeMode._
import main.scala.classes.LocationType._
import org.squeryl.Table
import ActiveProductMethod._
import scala.collection.mutable.ListBuffer
import java.util.Date
import ProductMethod._

//import main.scala.classes.LocationType

//import main.scala.classes.{Product, Location}
import collection.mutable.{Set,Map}

//Warehouse Class
class Warehouse(locationName:String, locationType:LocationType) extends Location (locationName:String, locationType:LocationType){

  //AddWarehouse Event
  /*def AddWarehouse(LocationID:Long):Warehouse = {
    require(PoSWare.Location.contains(LocationID))
    require(!PoSWare.Store.contains(LocationID))
    require(!PoSWare.Warehouse.contains(LocationID))
    PoSWare.Warehouse += LocationID //add to database
    new Warehouse()
  } */
  def this(locationName: String) = this(locationName, LocationType.location)
    /*
  //TODO Convert to Squeryl
  def SupplierOrder(Product:Product, amount:Int){
    require(PoSWare.Warehouse.contains(this.id))
    require(PoSWare.Product.contains(Product.id))
    require(PoSWare.ActiveProd.contains(this->Product))
    require(amount > 0)
    PoSWare.Order.update(Set(this->Product), amount)
  }

  //TODO Convert to Squeryl
  def SetStockAlertLevel(Product:Product, level:Int) {
    require(PoSWare.Warehouse.contains(this.id))
    require(PoSWare.Product.contains(Product.id))
    require(level > 0)
    require(PoSWare.ActiveProd.contains(this->Product))
    PoSWare.ReorderLevel.update(Set(this->Product), level)
  }

  //TODO Convert to Squeryl
  def LowStockAlert(Product:Product, level:Int, stock:Int) {
    require(PoSWare.Warehouse.contains(this.id))
    require(PoSWare.Product.contains(Product.id))
    require(PoSWare.ActiveProd.contains(this->Product))
    require(stock == PoSWare.Stock.apply(Set(this->Product)))
    require(stock < level)
    require(level == PoSWare.ReorderLevel.apply(Set(this->Product)))
    PoSWare.Order.update(Set(this->Product), level)
  }*/


}
object WarehouseMethod {
  //AddWarehouse
  def newWarehouse(locationName: String): Long= inTransaction{
    val newwarehouse = new Warehouse(locationName)
    //Warehouse := Warehouse U {warehouse}
    warehouseTable.insert(newwarehouse)
    return newwarehouse.id
  }


  import ActiveProductMethod._
  def productStockAtLocation (productName: String, locationId: Long): Long = inTransaction{
    val products = getVendingProduct(productName, locationId)
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
      val p = newProduct(productName,locationId, expiryDate)
      products += p
      i = i - 1
    }
    return products.toList
  }

  def printAll() {
    for (l <- from(warehouseTable)(a => select(a))) {
      println(l.id + " " + l.locationName + " " + l.locationType)
    }
  }

}