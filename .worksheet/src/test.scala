object test {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(57); 
  println("Welcome to the Scala worksheet");$skip(84); 
  val dd = try {
    val x = 42 + 5
    x + 2
  } catch { case e: Exception => 43 };System.out.println("""dd  : Int = """ + $show(dd ))}

}
