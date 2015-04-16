package tip

import java.io.{ PrintWriter, File }

object OutputKind extends Enumeration {
  val Cfg, Ast, Constraints , Sign, Liveness = Value
}

object Utils {

  /**
   * Generate an output for a file into a new file with the given id in the outFolder.
   * The content is written into the output, assuming the funciton is writing a specific kind of output
   *
   */
  def output(file: File, id: String, kind: OutputKind.Value, content: String, outFolder: File): Unit = {

    val extension = kind match {
      case OutputKind.Cfg => "_cfg.dot"
      case OutputKind.Sign => "_cfg_sign.dot"
      case OutputKind.Ast => "_type.ttip"
      case OutputKind.Liveness => "_liveness.dot"
      case OutputKind.Constraints => "_constraints.md"
    }
    val outFile = new File(outFolder, file.getName + id + extension)
    val pw = new PrintWriter(outFile, "UTF-8")
    pw.write(content)
    pw.close()

    println(s"$kind for $file $id written into ${outFile}")

  }

  /**
   * Escapes special characters in the given string.
   * Special characters are all Unicode chars except 0x20-0x7e but including \, ", {, and }.
   */
  def escape(s:String):String = {
    if (s == null)
      return null
    val  b = new StringBuilder()
    var i = 0
    for(i <- 0 to s.length-1){
      val c = s.charAt(i)
      c match {
        case '"' =>
        b.append("\\\"")
        case '\\' =>
          b.append("\\\\")
        case '\b' =>
          b.append("\\b")
        case '\t' =>
          b.append("\\t")
        case '\n' =>
          b.append("\\n")
        case '\r' =>
          b.append("\\r")
        case '\f' =>
          b.append("\\f")
        case '<' =>
          b.append("\\<")
        case '>' =>
          b.append("\\>")
        case '{' =>
          b.append("\\{")
        case '}' =>
          b.append("\\}")
        case _ =>
        if (c >= 0x20 && c <= 0x7e)
          b.append(c)
        else {
          b.append("\\u")
          val t = Integer.toHexString(c & 0xffff);
          var j = 0
          while(j + t.length() < 4) {
            b.append('0');
            b.append(t);
            j+=1
          }
        }
      }
    }
    return b.toString();
  }
}
