import java.util.Date
import java.text.SimpleDateFormat
import java.io.File
import scala.util.matching.Regex

class Parser(logFilename: String) {

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
      }

      (combat, it)
    }
  }

  class ActionParseException(val line: String) extends Throwable

  type AfterActionCallback = Combat => ()
  type AfterCombatCallback = Combat => ()

  def parse(): List[Combat] = {
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
