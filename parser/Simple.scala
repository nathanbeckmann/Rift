object Simple {
  def main(args: Array[String]) {
    if (args.length != 2) {
      println("Format: <combat-log-file> <delete-log?>")
    } else {

      println("Starting simple RIFT parser...")

      val parser = new Parser(args(0),
        c => (), //println(c.actions.tail),
        c => { println(c format "Length: %t,%1D\nDPS:%20d\nHPS:%20h\n");
               if (c.duration > 0) Clippy.copy(c toString) }
        )

      if (args(1).toBoolean)
        parser.delete()
      val list = parser.parse()

//       val list = parser.parseList()

//       for (combat <- list) {
//         println("Combat lasting " + (combat.duration / 1000) + " seconds:")
//         for (ent <- combat.entities.values) {
//           if (ent.dps > 0 || ent.hps > 0)
//             println(ent.name + " dps: " + ent.dps + " hps: " + ent.hps)
//         }
//       }
    }
  }
}
