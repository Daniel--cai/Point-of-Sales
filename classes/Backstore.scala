package main.scala.classes

import main.scala.classes.LocationType._


class Backstore(locationName: String, locationType: LocationType) extends Store (locationName: String, locationType: LocationType){
  def this() = this("", LocationType.backStore)
}
