object Simple {
  def main(args: Array[String]) {
    if (args.length != 1) {
      println("Format: <combat-log-file>")
    } else {
      val parser = new Parser(args(0))
      val list = parser.parse()

      for (combat <- list) {
        println("Combat lasting " + (combat.duration / 1000) + " seconds:")
        for (ent <- combat.entities.values) {
          if (ent.dps > 0 || ent.hps > 0)
            println(ent.name + " dps: " + ent.dps + " hps: " + ent.hps)
        }
      }
    }
  }
}
