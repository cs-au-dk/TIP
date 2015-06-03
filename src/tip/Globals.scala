package tip

import java.io.File

object Globals {

  val defaultOut = new File("./out")
  Globals.defaultOut.mkdirs()

}
