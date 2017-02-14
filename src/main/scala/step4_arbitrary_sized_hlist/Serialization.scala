package step4_arbitrary_sized_hlist

import shapeless._
import shapeless.syntax.std.product._

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
   ): CsvLineInstr[HEAD :: TAIL] = CsvLineInstr.instance {
		case head :: tail => instrHead.convert(head) :: instrTail.convert(tail)
	}

	//Prints some object of any type T as csv line
	//Precondition is, that an instruction object is provided, that creates a CsvLine from the object
	def printCsv[T](obj: T)(implicit instructions: CsvLineInstr[T]): Unit =
		println( instructions.convert(obj).mkString(",") )
}

case class Pizza(name:  String, size:     Int, price:  Double)
case class   CPU(model: String, version:  Int, ghz:    Double)
case class Fruit(name:  String, color: String, weight: Double, calories: Int)

object Serialization extends App { import CsvHelpers._

	val cheesePizza = Pizza("4cheeses", 32, 10.50)
	val cheesePizzaHList: String :: Int :: Double :: HNil = Generic[Pizza].to(cheesePizza)
	printCsv(cheesePizzaHList)

	val ryzen = CPU("AMD Ryzen", 3, 3.2)
	val ryzenHlist: String :: Int :: Double :: HNil = Generic[CPU].to(ryzen)
	printCsv(ryzenHlist)

	val redApple = Fruit("Apple", "red", 52.22, 100)
	val redAppleHlist: String :: String :: Double :: Int :: HNil = Generic[Fruit].to(redApple)
	printCsv(redAppleHlist)
}