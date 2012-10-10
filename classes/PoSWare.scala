package classes
import main.scala.classes._
import collection.mutable.{Set,Map}
import collection.mutable
/*
object PoSWare {

  //Initialise Store Set in PoSWare machine
  var Store:Set[(Long)] = Set()

  //Initialise location Set in PoSWare machine
  var Location:Set[(Long)] = Set()

  //Initialise Warehouse Set in PoSWare machine
  var Warehouse:Set[(Long)] = Set()

  //Initialise PRODUCT Set in PoSWare_ctx context
  //var PRODUCT:Set[(String)] = Set()

  //Initialise Product Set in PoSWare machine
  var Product:Set[(Long)] = Set()

  //Initialise ActiveProd Set in PoSWare machine
  var ActiveProd:Set[(Location,Product)] = Set()

  //Initialise Stock Set in PoSWare machine
  var Stock:Map[Set[(Location,Product)],Int] = Map()

  var Order:Map[Set[(Location,Product)], Int] = Map()

  var ReorderLevel:Map[Set[(Location,Product)], Int] = Map()




  //TODO extension in WarehouseR1
  //AddStore Event
  /*def AddStore(LocationID:Long):(Location,Location) = {
    require(PoSWare.Location.contains(LocationID))
    require(!PoSWare.Store.contains(LocationID))
    require(!PoSWare.Warehouse.contains(LocationID))
    PoSWare.Store += LocationID
    (new Backstore(), new Store())
  } */



  //StoreArea ∈ Store ↔ STORELOCATION
  var StoreArea:Map[Store,String] = Map()
  //	StoreAreaItem ∈ StoreArea ↔ Product
  var StoreAreaItem:Map[Map[Store,String], Product] = Map()
  //StoreAreaItemQuantity ∈ StoreAreaItem → ℕ
  var StoreAreaItemQuantity:Map[Map[Location,Store],Int] = Map()

  var Users:Set[(String)] = Set()
  var Auth:Map[String,String] = Map()

  //Backstore Class
  class Backstore() extends Location (){

  }

  //AddProductList Event
  /*def AddProductList(ProductID:String):Product = {
    require(!Product.contains(ProductID))
    Product += ProductID
    new Product(ProductID)
  }
  val AddProductList = (id:Long) => {
    require(!Product.contains(id))
    Product += id
    //new Product(id)
  } */
  /**
   * AddProductLocation Event
   * I'm not sure if it is meant to take in a Product class and Location class in respect to the Event-B.
   * But it is what I did.
   * @param Product
   *                A Product Class
   * @param Location
   *                A Location Class
   * @param stock
   *                An Integer, should be 0.
   */
  //TODO Convert to Squeryl
  def AddProductLocation(Product:Product, Location:Location, stock:Int) {
    require(PoSWare.Product.contains(Product.id))
    //require(PoSWare.Location.contains(Location.ID))
    require(PoSWare.Warehouse.contains(Location.id))
    require(!ActiveProd.contains(Location->Product))
    require(stock == 0)
    //@grd5 {location ↦ item} ∈ Location ↔ Product
    //require((Location->Product.ID).eq(PoSWare.Location.contains(Location.ID)->Product.ID))
    require(!PoSWare.Store.contains(Location.id))
    ActiveProd += (Location->Product)
    Stock += Set(Location->Product)->stock

    PoSWare.Order += (Set(Location->Product))->0
    PoSWare.ReorderLevel += (Set(Location->Product))->0
  }

}
*/
