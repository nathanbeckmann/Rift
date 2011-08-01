import java.io.{File,FileWriter,Writer}

object Simple {

  def postAction(c: Combat) {
    // nothing
  }

  def postCombat(c: Combat) {

    println(c format "Length: %t,%1D\nDPS:%20d\nHPS:%20h\n")

    if (c.duration > 0) Clippy.copy(c toString)

    if (Config.makeGraphs && c.entities.nonEmpty) {

      // only make graphs if some entity took more than 1 million damage
      val maxdmg = c.entities.values.map(_.damageTaken.full).max

      if (maxdmg > 1000000) {
      
        val filename = ("../data/" + Util.now + (c format "%1D") + ".txt") replaceAll (":", "-")

        val file = new File(filename)

        file.delete()

        if (file.createNewFile()) {
          val writer = new FileWriter(filename)

          try {
            writer write c.graphData

            println("Graph data saved to : " + filename)
          } finally {
            writer.close()
          }
        } else {
          println("Couldn't create graph data file! " + filename)
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
