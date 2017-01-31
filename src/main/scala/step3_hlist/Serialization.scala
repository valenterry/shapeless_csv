package step3_hlist

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

	//Instance or "instruction object" to convert any hlist with 3 elements A, B, C into a CsvLine
	implicit def hlistCsvLineInstr[A, B, C](
	   implicit instrA: CsvFieldInstr[A], instrB: CsvFieldInstr[B], instrC: CsvFieldInstr[C]
   ): CsvLineInstr[A :: B :: C :: HNil] = CsvLineInstr.instance { hlist =>
		instrA.convert(hlist(0)) ::
		instrB.convert(hlist(1)) ::
		instrC.convert(hlist(2)) ::
		Nil
	}

	//Prints some object of any type T as csv line
	//Precondition is, that an instruction object is provided, that creates a CsvLine from the object
	def printCsv[T](obj: T)(implicit instructions: CsvLineInstr[T]): Unit =
		println( instructions.convert(obj).mkString(",") )
}

case class Pizza(name:  String, size:    Int, price:  Double)
case class Tank (model: String, version: Int, weight: Double)

object Serialization extends App { import CsvHelpers._

	val cheesePizza = Pizza("4cheeses", 32, 10.50)
	val cheesePizzaHList: String :: Int :: Double :: HNil = Generic[Pizza].to(cheesePizza)
	printCsv(cheesePizzaHList)

	val leopard2 = Tank("Leopard", 2, 62.52)
	val leopard2Hlist: String :: Int :: Double :: HNil = Generic[Tank].to(leopard2)
	printCsv(leopard2Hlist)
}