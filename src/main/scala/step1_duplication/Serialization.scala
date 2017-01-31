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

	implicit val tankCsvLineInstr: CsvLineInstr[Tank] = CsvLineInstr.instance{ tank =>
		StringCsvField.convert(tank.model) ::
		IntCsvField.convert(tank.version) ::
		DoubleCsvField.convert(tank.weight) ::
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
	printCsv(cheesePizza)

	val leopard2 = Tank("Leopard", 2, 62.52)
	printCsv(leopard2)
}