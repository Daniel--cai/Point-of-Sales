package main.scala.classes

//import classes.PoSWare

import main.scala.classes.Database._
import org.squeryl.PrimitiveTypeMode._
import main.scala.classes.Database._
import org.squeryl.PrimitiveTypeMode._
import org.squeryl.Table

import scala.collection.mutable.ListBuffer
import java.util.Date
import ProductMethod._
import LocationType._
import ActiveProductMethod._


class Store(locationName: String, locationType: LocationType) extends Location (locationName:String, locationType:LocationType){

  def this(locationName: String) = this(locationName, LocationType.frontStore)

  def add() {
    frontstoreTable.insert(this)
  }

  def printAll() {
    for (l <- from(frontstoreTable)(a => select(a))) {
      println(l.id + " " + l.locationName + " " + l.locationType)
    }
  }

  //TODO Convert to Squeryl
  /*def RemProductStoreFront(Product:Product, Location:Location) {
    require(PoSWare.Product.contains(Product.id))
    require(PoSWare.Store.contains(Location.id))
    require(!PoSWare.Warehouse.contains(Location.id))
    require(Set(Set(Location->Product)).contains(PoSWare.ActiveProd.union(Set(Location->Product))))  //@grd4 ActiveProd ∪ {store ↦ item} ∈ Location ↔ Product
    require(PoSWare.ActiveProd.contains(Location->Product))
    PoSWare.ActiveProd -= Location->Product
    //TODO @act2 StoreAreaItemQuantity ≔ StoreAreaItemQuantity {(store ↦ Frontstore ↦ item) ↦ 0, (store ↦ Backstore ↦ item) ↦ 0}
    //StoreAreaItemQuantity.update(Map(Location,Location),0)
    PoSWare.Stock.update(Set(Location->Product),0)
  }

  //TODO Convert to Squeryl
  //TODO  AddProductStoreFront Event
  def AddProductStoreFront(Product:Product, Location:Location, Stock:String) {
    require(PoSWare.Product.contains(Product.id))
    require(PoSWare.Store.contains(Location.id))
    require(PoSWare.Warehouse.contains(Location.id))
    require(Set(Set(Location->Product)).contains(PoSWare.ActiveProd.union(Set(Location->Product)))) //@grd4 ActiveProd ∪ {store ↦ item} ∈ Location ↔ Product
  }

  def LowStockAlertStore(Product:Product, Location:Location, level:Int, stock:Int) {
    require(PoSWare.Store.contains(Location.id))
    require(PoSWare.Product.contains(Product.id))
    require(level == PoSWare.ReorderLevel.apply(Set(Location->Product)))
    require(PoSWare.ActiveProd.contains(Location->Product))
    require(stock == PoSWare.Stock.apply(Set(Location->Product)))
    require(stock < level)
    PoSWare.Order.update(Set(Location->Product),level)
  }

  def SetStockAlertLevelStore(Product:Product, Location:Location, level:Int, user:String) {
    require(PoSWare.Store.contains(Location.id))
    require(PoSWare.Product.contains(Product.id))
    require(level > 0)
    require(PoSWare.ActiveProd.contains(Location->Product))
    require(PoSWare.Users.contains(user))
    require(PoSWare.Auth.apply(user) == "Manager")
    PoSWare.ReorderLevel.update(Set(Location->Product),level)
  } */
}

object StoreMethod {
  def RestockFrontStore(locationId: Long, warehouseId: Long, productName: String, qty: Long): List[Long] = inTransaction{
    val s = frontstoreTable.where(s => s.id === locationId).single
    //location : Warehouse
    //item : Product
    //{location |-> item} :< ActiveProd
    if (!s.isInstanceOf[Store]){
      println("Error: Location not a store")
      return null
    }
    val w = warehouseTable.where(w => w.id === locationId).single
    if (!w.isInstanceOf[Store]){
      println("Error: Location Source not a warehouse")
      return null
    }
    if (!activeProductExists(productName)){
      println("Error: "+productName + " does not exist yet. Add a new product?")
      return null
    }
    val products = new ListBuffer[Long]
    var i: Int = qty.toInt
    while(i > 0){
      //val p = moveProduct(productName,locationId, expiryDate)
      //products += p
      i = i - 1
    }
    return products.toList
  }

}