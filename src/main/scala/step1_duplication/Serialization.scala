package step1_duplication

object CsvHelpers {
	//Some instances or "instruction objects" to convert common types into CsvFields
	implicit val StringCsvField: CsvFieldInstr[String] =
		CsvFieldInstr.instance(string => string)

	implicit val IntCsvField:    CsvFieldInstr[Int]    =
		CsvFieldInstr.instance(int => int.toString)

	implicit val DoubleCsvField: CsvFieldInstr[Double] =
		CsvFieldInstr.instance(double => double.toString)

	//Instances or "instruction objects" to convert case classes into CsvLines
	implicit val pizzaCsvLineInstr: CsvLineInstr[Pizza] = CsvLineInstr.instance{ pizza =>
		StringCsvField.convert(pizza.name) ::
		IntCsvField.convert(pizza.size) ::
		DoubleCsvField.convert(pizza.price) ::
		Nil
	}

	implicit val cpuCsvLineInstr: CsvLineInstr[CPU] = CsvLineInstr.instance{ cpu =>
		StringCsvField.convert(cpu.model) ::
		IntCsvField.convert(cpu.version) ::
		DoubleCsvField.convert(cpu.ghz) ::
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
	printCsv(cheesePizza)

	val ryzen = CPU("AMD Ryzen", 3, 3.2)
	printCsv(ryzen)
}