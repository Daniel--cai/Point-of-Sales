package main.scala.classes

import org.squeryl.PrimitiveTypeMode._
import org.squeryl.Schema
import org.squeryl.annotations
import org.squeryl.dsl.OneToMany
import org.squeryl.adapters.H2Adapter
import java.util.Date
import java.sql.Timestamp
import main.scala.classes.Database._
import LocationType._
import WarehouseMethod._

class Location (val locationName: String, var locationType: LocationType) extends Basic {
  lazy val products: OneToMany[Product] = locationToProduct.left(this)
  def this(locationName: String) = this(locationName, LocationType.location)
}

object LocationMethod {

  def newLocation(locationName: String):Long = inTransaction {
    val newlocation = new Location(locationName)
    locationTable.insert(newlocation)
    return newlocation.id
  }

  def setLocAsWarehouse(locationId: Long) :Long = inTransaction{

    val newwarehouse = new Warehouse(getLocationName(locationId), locationId)
    //Warehouse := Warehouse U {warehouse}
    warehouseTable.insert(newwarehouse)
    update(locationTable)(l=>
      where(l.id === locationId)
      set(l.locationType := LocationType.warehouse))
    return newwarehouse.id
  }

  def setLocAsBackstore(locationId: Long) :Long = inTransaction{
    //   warehouseTable.insert()
    return 0   //warehouse id
  }

  //def productsToRestock(productName: String) =
  //  from(locationTable, productTable)(l,p) =>
  //    where(l.id === this.id
  //    and p.productName === productName)
  //    select(p))

  def getLocationName(locationId: Long):String = inTransaction{
    val l = locationTable.where(l=>l.id === locationId).single
    return l.locationName
  }

  def getAllLocation() : List[Location] = inTransaction{
    val l = from(locationTable)(l => select(l))
    return l.toList
  }

  def printAll() = inTransaction {
    for (l <- from(locationTable)(a => select(a))) {
      println(l.id + " " + l.locationName + " " + l.locationType)
    }
  }
  import LocationType._
  def getLocationType (locationId: Long):LocationType = inTransaction{
    val loc = locationTable.where(l=>l.id === locationId).single
    return loc.locationType
  }

}
