import java.io.{File,FileWriter,Writer}

object Simple {

  def postAction(c: Combat) {
    // nothing
  }

  def postCombat(c: Combat) {

    println(c format "Length: %t,%1D\nDPS:%20d\nHPS:%20h\n")

    if (c.duration > 0) Clippy.copy(c toString)

    if (Config.makeGraphs) {
      
      val filename = ("../data/" + Util.now + (c format "%1D") + ".txt") replaceAll (":", "-")

      val file = new File(filename)

      if (file.createNewFile()) {
        val writer = new FileWriter(filename)
        
        try {
          c.writeGraphData(writer)
        } finally {
          writer.close()
        }
      }
    }
  }

  def main(args: Array[String]) {
    if (args.length != 3) {
      println("Format: <combat-log-file> <dynamic?> <make-graphs?>")
      println
      println("If dynamic is 'true', then the existing log file will be deleted.")
      println("If it is false, then the parser will exit when end of file is reached.")
    } else {

      println("Starting simple RIFT parser...")

      val parser = new Parser(args(0),
                              postAction,
                              postCombat)

      val live = args(1).toBoolean

      if (args(2).toBoolean)
        Config.makeGraphs = true

      if (live) {
        parser.delete()
        parser.parse()
      } else {
        val list = parser.parseList()
      }
    }
  }
}
