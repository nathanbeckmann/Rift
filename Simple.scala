import java.util.Date
import java.text.SimpleDateFormat
import java.io.File
import scala.collection.mutable.Map
import scala.collection.mutable.DoubleLinkedList
import scala.util.matching.Regex

class Entity(
  val name: String,
  val combat: Combat) {

  val actions: DoubleLinkedList[Action] = new DoubleLinkedList[Action]()

  var damage: Double = 0
  var heals: Double = 0
  var damageTaken: Double = 0
  var healsTaken: Double = 0

  def dps: Double = damage / combat.duration
  def hps: Double = heals / combat.duration
  def dtps: Double = damageTaken / combat.duration
  def htps: Double = healsTaken / combat.duration
}

class Action (
  val time: Date,
  val source: String,
  val target: String,
  val name: String,
  val amount: Double,
  val category: Int) {

  object Categories {
    val CAST_START = 1
    val HIT = 3
    val DOT = 4
    val HEAL = 5
    val BUFF_START = 6
    val BUFF_END = 7
    val DEBUFF_START = 8
    val DEBUFF_END = 9
    val MISS = 10
    val SLAIN = 11
    val DODGE = 15
    val CRITICAL_HIT = 23
    val IMMUNE = 26
    val REGEN = 27                        // ??
    val CRITICAL_HEAL = 28
  }
  import Categories._

  def isDmg: Boolean = category match {
    case HIT => true
    case DOT => true
    case CRITICAL_HIT => true
    case _ => false
  }

  def isHeal: Boolean = category match {
    case HEAL => true
    case CRITICAL_HEAL => true
    case _ => false
  }
}

class Combat {
  def start: Long = actions.head.time.getSeconds
  def end: Long = actions.last.time.getSeconds
  def duration: Long = end - start

  var actions: DoubleLinkedList[Action] = new DoubleLinkedList[Action]()

  val entities = Map[String, Entity]()
}

class Parser(logFilename: String) {

  private val extractTime = """(\d\d:\d\d:\d\d):? (.*)""".r
  private val extractAction = """\( (\d+) .* , (.*) , (.*) , (\d+) , \d+ , (.*) \) .*""".r

  class ActionParseException(val line: String) extends Throwable

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

        // extending Map to use default didn't work for some reason
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

        it = it drop 1
      }

      (combat, it)
    }
  }

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

object Simple {
  def main(args: Array[String]) {
    if (args.length != 1) {
      println("Format: <combat-log-file>")
    } else {
      val parser = new Parser(args(0))
      val list = parser.parse()

      for (combat <- list;
           ent <- combat.entities.values) {
        println(ent.name + ", dps: " + ent.dps + ", hps: " + ent.hps)
      }
    }
  }
}
