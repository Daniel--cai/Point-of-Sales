package main.scala.classes

//import classes.PoSWare.Backstore
import org.squeryl.PrimitiveTypeMode._
import org.squeryl.Schema
import org.squeryl.annotations
import org.squeryl._
import adapters.H2Adapter
import java.util.Date
import java.text.SimpleDateFormat
import java.sql.Timestamp
import swing._

object UserType extends Enumeration {
  type UserType = Value
  val clerk = Value("Clerk")
  val manager = Value("Manager")
  val admin = Value("Admin")
}

object LocationType extends Enumeration {
  type LocationType = Value
  val location = Value(1,"Location")
  val warehouse = Value(2,"Warehouse")
  val frontStore = Value(3,"FrontStore")
  val backStore = Value(4,"BackStore")
}

object ProductStatus extends Enumeration {
  type ProductStatus = Value
  val vending = Value(1, "Vending")
  val reserved = Value(2, "Reserved")
  val sold = Value(3, "Sold")
  val scanned = Value(4, "Scanned")
  //val damaged = Value(5, "Damaged")
}

object Database extends Schema {
  val productTable = table[Product]("product")
  val locationTable = table[Location]("location")
  val warehouseTable = table[Warehouse]("warehouse")
  val frontstoreTable = table[Store]("store")
  val backstoreTable = table[Backstore]("backstore")
  val userTable = table[User]("user")
  val memberTable = table[Member]("member")
  val reserveTable = table[Reserve]("reserve")
  val transactionTable = table[Transaction]("transaction")
  val purchaseTable = table[Purchase]("purchase")
  val reservedproductTable = table[ReservedProduct]("reservedproduct")
  val activeproductTable = table[ActiveProduct]("activeproduct")
  val locationToProduct = oneToManyRelation(locationTable, productTable).via((l,p) => l.id === p.locationId )
  val warehouseToProduct = oneToManyRelation(warehouseTable, productTable).via((l,p) => l.id === p.locationId )
}

object GUI extends SimpleSwingApplication{
  import event._
  def top = new MainFrame {
    interface.setup
    title = "Add Location"
    val LocationInput = new TextField
    val ProductNameInput = new TextField
    val LocationIdInput = new TextField
    val LocationLabel = new Label{
      text = "Location:"
      border = Swing.EmptyBorder(5,5,5,5)
    }
    val LocationTypeInput = new TextField
    val LocationType = new Label{
      text = "Type:"
      border = Swing.EmptyBorder(5,5,5,5)
    }

    val convertButton = new Button {
      text = "Add Location"
    }

    val printButton = new Button {
      text = "Print"
    }
    val output = new Label{
      text = ""
      border = Swing.EmptyBorder(20,20,20,20)
      listenTo(convertButton,LocationInput)
      def add() {
        val location = LocationInput.text
        text = location + " added in successfully!"
        println("added successfully")
      }
      reactions += {
        case ButtonClicked(_) => add()
      }
    }
    contents = new GridPanel(6,4) {
      contents.append(LocationLabel,LocationInput,ProductNameInput, LocationIdInput,LocationType,LocationTypeInput,convertButton,printButton,output)
      border = Swing.EmptyBorder(10,10,10,10)
    }

  }
}

object interface{
  import Database._
  import org.squeryl.SessionFactory
  import main.scala.classes._

  def setup {

    Class.forName("org.h2.Driver");
    SessionFactory.concreteFactory = Some(()=>
      Session.create(
        java.sql.DriverManager.getConnection("jdbc:h2:~/example", "sa", ""),
        new H2Adapter)
    )
    inTransaction {
      drop
      create
      printDdl
    }
  }
}

object Main {
  import Database._
  import org.squeryl.SessionFactory
  import ProductMethod._
  import ActiveProductMethod._
  def main(args: Array[String]) {

         Class.forName("org.h2.Driver");
    SessionFactory.concreteFactory = Some(()=>
      Session.create(
        java.sql.DriverManager.getConnection("jdbc:h2:~/PointOfSales", "sa", ""),
        new H2Adapter)
    )

    inTransaction {

      drop
      create
      printDdl
    }
      import UserMethod._
      import LocationMethod._
      import ProductMethod._
      import ActiveProductMethod._
      import MemberMethod._
      import TransactionMethod._
      import WarehouseMethod._
      val use1 = newUser("Daniel", UserType.admin)
      println("Usertype of Daniel is "+getUserType(use1))

      val loc1 = newLocation("A")
      val loc2 = newLocation("B")
      val war5 = newWarehouse("Z")
      LocationMethod.printAll()


      val dateFormat = new SimpleDateFormat("dd-MM-yyyy")

      val milk = newActiveProduct("Milk", 10)

      println("Milk exists "+activeProductExists("Milk"))
      println("Id of milk is " +getActiveProductId("Milk"))
      val milk1 = newProduct("Milk", loc1, LocationType.location,dateFormat.parse("02-01-1992"))
      val milk2 = newProduct("Milk", loc1, LocationType.location,dateFormat.parse("02-01-1992"))
      val cheese = newProduct("Milk", war5, LocationType.warehouse,dateFormat.parse("02-01-1992"))
      //moveProductLocation(milk2, loc2)

      val mem1 = newMember("Daniel Cai")
      val tra1 = newTransaction(use1, mem1)
      println("user name of tra1 is "+getUserName(getTransactionUser(tra1)))
      ProductMethod.printAll()
      addProductQtyToTransaction(tra1, "Milk", 1, loc1, LocationType.location)
      val war2 = newWarehouse("C")
      val war1 = newWarehouse("D")

      println("war1 "+ war1 + "loc 1 " + loc1)
      println("Total Milk at loc1: "+ productStockAtLocation("Milk", loc1))
      println("Total Milk at warehouse: "+ productStockAtLocation("Milk", war1))
      RestockWarehouse(war1,"Milk",  dateFormat.parse("02-10-2012"), 20)
      println("Total Milk at warehouse after restocking: "+ productStockAtLocation("Milk", war1))
      addProductQtyToTransaction(tra1, "Milk", 1, loc1, LocationType.location)
      println("Total Milk at loc1 after purchasing: "+ productStockAtLocation("Milk", loc1))
      val lists = getProductsInTransaction(tra1)
      for (l <- lists){

        print(getProductName(l)+ " ")
      }
      println("")
      printf("total cost: %.2f\n", calculateTotal(tra1))
  }
}