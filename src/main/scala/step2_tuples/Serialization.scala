package step2_tuples

object CsvHelpers {
	//Some instances or "instruction objects" to convert common types into CsvFields
	implicit val StringCsvField: CsvFieldInstr[String] =
		CsvFieldInstr.instance(string => string)

	implicit val IntCsvField:    CsvFieldInstr[Int]    =
		CsvFieldInstr.instance(int => int.toString)

	implicit val DoubleCsvField: CsvFieldInstr[Double] =
		CsvFieldInstr.instance(double => double.toString)

	//Instance or "instruction object" to convert any tuple (A, B, C) into a CsvLine
	implicit def tupleCsvLineInstr[A, B, C](
	   implicit instrA: CsvFieldInstr[A], instrB: CsvFieldInstr[B], instrC: CsvFieldInstr[C]
   ): CsvLineInstr[(A, B, C)] = CsvLineInstr.instance { tuple =>
		instrA.convert(tuple._1) ::
		instrB.convert(tuple._2) ::
		instrC.convert(tuple._3) ::
		Nil
	}

	//Prints some object of any type T as csv line
	//Precondition is, that an instruction object is provided, that creates a CsvLine from the object
	def printCsv[T](obj: T)(implicit instructions: CsvLineInstr[T]): Unit =
		println( instructions.convert(obj).mkString(",") )
}

case class Pizza(name:  String, size:    Int, price:  Double)
case class   CPU(model: String, version: Int, ghz:    Double)


object Serialization extends App { import CsvHelpers._

	val cheesePizza = Pizza("4cheeses", 32, 10.50)
	val cheesePizzaTuple: (String, Int, Double) = Pizza.unapply(cheesePizza).get
	printCsv(cheesePizzaTuple)

	val ryzen = CPU("AMD Ryzen", 3, 3.2)
	val ryzenTuple: (String, Int, Double) = CPU.unapply(ryzen).get
	printCsv(ryzenTuple)
}