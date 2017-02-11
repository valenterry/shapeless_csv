package step6_support_sumtypes

import CsvFieldInstr.CsvField
import CsvLineInstr.CsvLine

//A Typeclass to serialize some object of type T into a csv field by passing it to the convert method
trait CsvFieldInstr[T] { def convert(obj: T): CsvField }
//Helper object that makes it convenient to create instances of the CsvFieldInstr typeclass
object CsvFieldInstr {
	//Type alias to have a more self-documenting type signature
	type CsvField = String
	def instance[T](f: T => CsvField) = new CsvFieldInstr[T] {
		override def convert(obj: T): CsvField = f(obj)
	}
}
//A Typeclass to serialize some object of type T into a list of csv fields (= a csv line) by passing it to the convert method

trait CsvLineInstr[T] { def convert(obj: T): CsvLine }
//Helper object that makes it convenient to create instances of the CsvLineInstr typeclass
object CsvLineInstr {
	type CsvLine = List[CsvField]
	def instance[T](f: T => CsvLine) = new CsvLineInstr[T] {
		override def convert(obj: T): CsvLine = f(obj)
	}
}