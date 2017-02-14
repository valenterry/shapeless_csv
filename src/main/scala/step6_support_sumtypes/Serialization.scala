package step6_support_sumtypes

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
   ): CsvLineInstr[HEAD :: TAIL] = CsvLineInstr.instance { hnil =>
		instrHead.convert(hnil.head) :: instrTail.convert(hnil.tail)
	}

	//Instance or "instruction object" to convert the "end" of a sumtype into a CsvField
	//There are no instances of sealed trait CNil but the compiler does not understand this.
	//Thus we still need to proof to the compiler that we could, theoretically, serialize a CNil instance
	implicit val CNilCsvLine:	 CsvLineInstr[CNil]    =
		CsvLineInstr.instance(cnil => throw new Exception("There can't be instances of type CNil!"))

	//Instance or "instruction object" to convert a Coproduct aka sumtype with at least one element into a CsvField
	implicit def coproductCsvFieldInstr[HEAD, TAIL <: Coproduct](
		implicit instrHead: CsvLineInstr[HEAD], instrTail: CsvLineInstr[TAIL]
	): CsvLineInstr[HEAD :+: TAIL] = CsvLineInstr.instance {
		//Inl = "In Left" => The concrete type for the Coproduct is, in this case, HEAD
		case Inl(head) => instrHead.convert(head)
		//Inr = "In right" => The concrete type is not HEAD, it is one of the other types that are stored in the TAIL
		case Inr(tail) => instrTail.convert(tail)
	}

	//Creates Instances or "instruction objects" for any Type T
	//Precondition is, that we have instructions how to create a generic representation from type T and that we have instructions to serialize it
	implicit def genericCsvLineInstr[T, Ts_GENERIC_REPRESENTATION](
		implicit tToGenericInstr: Generic.Aux[T, Ts_GENERIC_REPRESENTATION], genInstr: Lazy[CsvLineInstr[Ts_GENERIC_REPRESENTATION]]
	): CsvLineInstr[T] = CsvLineInstr.instance { obj =>
		val genericRepr: Ts_GENERIC_REPRESENTATION = tToGenericInstr.to(obj)
		genInstr.value.convert(genericRepr)
	}

	//Prints some object of any type T as csv line
	//Precondition is, that an instruction object is provided, that creates a CsvLine from the object
	def printCsv[T](obj: T)(implicit instructions: CsvLineInstr[T]): Unit =
		println( instructions.convert(obj).mkString(",") )
}

sealed trait Food
case class Pizza(name:  String, size:     Int, price:  Double) extends Food
case class Fruit(name:  String, color: String, weight: Double, calories: Int) extends Food
case class Cola (brand: String, iceCubes: Int) extends Food

object Serialization extends App { import CsvHelpers._
	val foods: List[Food] = List(
		Pizza("4cheeses", 32, 10.50),
		Fruit("Apple", "red", 52.22, 100),
		Fruit("Orange", "orange", 80, 120),
		Cola ("Coca Cola", 3)
	)

	foods.foreach(food => printCsv(food))
}