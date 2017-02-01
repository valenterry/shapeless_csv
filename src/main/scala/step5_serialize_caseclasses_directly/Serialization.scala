package step5_serialize_caseclasses_directly

import shapeless._
import shapeless.syntax.std.product._

case class Pizza(name:  String, size:     Int, price:  Double)
case class Tank (model: String, version:  Int, weight: Double)
case class Fruit(name:  String, color: String, weight: Double, calories: Int)
case class Plate(i: Int, p: Pizza)

object CsvHelpers {
	//Some instances or "instruction objects" to convert common types into CsvFields
	implicit val StringCsvField: CsvFieldInstr[String] =
		CsvFieldInstr.instance(string => string)

	implicit val IntCsvField:    CsvFieldInstr[Int]    =
		CsvFieldInstr.instance(int => int.toString)

	implicit val DoubleCsvField: CsvFieldInstr[Double] =
		CsvFieldInstr.instance(double => double.toString)

	//Instance or "instruction object" to convert an empty hlist into a CsvField
	implicit val HNilCsvLine:	 CsvLineInstr[HNil]    =
		CsvLineInstr.instance(hnil => Nil)

	//Instance or "instruction object" to convert hlists with at least one element into a CsvLine
	implicit def hlistCsvLineInstr[HEAD, TAIL <: HList](
	   implicit instrHead: CsvFieldInstr[HEAD], instrTail: CsvLineInstr[TAIL]
   ): CsvLineInstr[HEAD :: TAIL] = CsvLineInstr.instance { hnil =>
		instrHead.convert(hnil.head) :: instrTail.convert(hnil.tail)
	}

	//Creates Instances or "instruction objects" for any Type T
	//Precondition is, that we have instructions how to create an HList from type T and that we have instructions to serialize this HList
	implicit def genericCsvLineInstr[T, Ts_HLIST_REPRESENTATION](
		implicit tToHListInstr: Generic[T] {type Repr = Ts_HLIST_REPRESENTATION}, hlistInstr: CsvLineInstr[Ts_HLIST_REPRESENTATION]
	): CsvLineInstr[T] = CsvLineInstr.instance { obj =>
		val hlist: Ts_HLIST_REPRESENTATION = tToHListInstr.to(obj)
		hlistInstr.convert(hlist)
	}

	//Prints some object of any type T as csv line
	//Precondition is, that an instruction object is provided, that creates a CsvLine from the object
	def printCsv[T](obj: T)(implicit instructions: CsvLineInstr[T]): Unit =
		println( instructions.convert(obj).mkString(",") )
}

object Serialization extends App { import CsvHelpers._
	val cheesePizza = Pizza("4cheeses", 32, 10.50)
	printCsv(cheesePizza)

	val leopard2 = Tank("Leopard", 2, 62.52)
	printCsv(leopard2)

	val redApple = Fruit("Apple", "red", 52.22, 100)
	printCsv(redApple)
}