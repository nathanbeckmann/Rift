import java.util.Date
import java.text.SimpleDateFormat
import java.io.File
import scala.util.matching.Regex

class Parser(
  logFilename: String,
  afterActionCallback: Combat => Unit,
  afterCombatCallback: Combat => Unit) {

  class ActionParseException(val line: String) extends Throwable

  private val extractTime = """(\d\d:\d\d:\d\d):? (.*)""".r
  private val extractAction = """\( (\d+) .* , (.*) , (.*) , (\d+) , \d+ , (.*) \) .*""".r

  // returns true if still in combat
  private def parseAction(line: String): Action = {

    val extractTime(timeStr, action) = line
    val time = new SimpleDateFormat("HH:mm:ss").parse(timeStr)

    action match {
      case "Combat Begin" => new Action(time, "", "", action, 0, 0)
      case "Combat End" => new Action(time, "", "", action, 0, 0)
      case extractAction(category, source, target, amount, name) =>
        new Action(time, source, target, name, amount.toDouble, category.toInt)
      case _ =>
        throw new ActionParseException(line)
    }
  }

  private def parseCombat(lines: Iterator[String]): (Combat, Iterator[String]) = {
    var it = lines dropWhile (l => !(l contains "Combat Begin"))

    if (!it.hasNext)
      (null, it)
    else {
      val combat = new Combat
      var inCombat = true

      while (inCombat) {

        // wait for a line to be available
        while (!it.hasNext)
          Thread.sleep(500)

        val action = parseAction(it.next)
        inCombat = action.name != "Combat End"

        combat.actions = combat.actions :+ action

        // extending Map to use default didn't work for some reason, probably I'm a nub and a moron
        if (!(combat.entities contains action.source)) combat.entities += action.source -> new Entity(action.source, combat)
        if (!(combat.entities contains action.target)) combat.entities += action.target -> new Entity(action.target, combat)

        val source = combat.entities(action.source)
        val target = combat.entities(action.target)
        source.actions :+ action

        if (action.isDmg) {
          source.damage += action.amount
          target.damageTaken += action.amount
        }
        else if (action.isHeal) {
          source.heals += action.amount
          target.healsTaken += action.amount
        }

        afterActionCallback(combat)
      }

      afterCombatCallback(combat)

      (combat, it)
    }
  }

  // On-line processing of logs; does not store each combat as it is
  // parsed
  def parse() {

    val log = new File(logFilename)

    // First, remove the old log
    if (!log.delete())
      println("Couldn't delete the file!")

    // Start parsing...
    val lines = scala.io.Source.fromFile(log).getLines
    while (true)
      parseCombat(lines)
  }

  // Off-line processing of logs
  def parseList(): List[Combat] = {
    val log = new File(logFilename)
    val lines = scala.io.Source.fromFile(log).getLines

    def makeCombatList(it: Iterator[String]): List[Combat] = {
      val (combat, nextLines) = parseCombat(it)
      if (combat != null)
        combat :: makeCombatList(nextLines)
      else
        Nil
    }

    makeCombatList(lines)
  }
}
