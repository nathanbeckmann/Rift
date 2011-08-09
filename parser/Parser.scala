import java.util.Date
import java.text.SimpleDateFormat
import scala.util.matching.Regex
import java.io.File

class Parser(
  logFilename: String,
  afterActionCallback: Combat => Unit,
  afterCombatCallback: Combat => Unit) {

  import Parser._

  // This extracts the timestamp
  private val extractTime = """(\d\d:\d\d:\d\d):? (.*)""".r

  // This extracts...
  // - action type
  // - source, target, and owner ids
  // - source and owner names
  // - action value (dmg, heal)
  // - action name
  private val extractAction = """\( (\d+) , (.*) , (.*) , (.*) , .* , (.*) , (.*) , (-?\d+) , \d+ , (.*) \) (.*)""".r
  private val extractAbsorbed = """.*\((\d+) absorbed\)""".r
  private val timeFormat = new SimpleDateFormat("HH:mm:ss")

  // returns true if still in combat
  private def parseAction(line: String): Action = {

    val extractTime(timeStr, action) = line
    val time = timeFormat.parse(timeStr)

    action match {
      case "Combat Begin" => new Action(time, Id(), Id(), Id(), "", "", action, 0, 0)
      case "Combat End" => new Action(time, Id(), Id(), Id(), "", "", action, 0, 0)
      case extractAction(category, sourceId, targetId, ownerId, source, target, amount, name, text) =>

        val absorbed = text match {
            case extractAbsorbed(abs) if Config.includeAbsorbed => abs.toDouble
            case _ => 0
          }

        new Action(time, Id(sourceId), Id(targetId), Id(ownerId),
                   source, target, name, amount.toDouble + absorbed, category.toInt)
      case _ =>
        throw new ParseError(line)
    }
  }

  private def waitForLine(it: Iterator[String], timeout: Long = 0): Boolean = {
    val start = new Date()

    // wait for a line to be available
    while (!it.hasNext) {
      Thread.sleep(500)

      if (timeout > 0 &&
          ((new Date()).getTime - start.getTime) > timeout)
        return false
    }

    return true
  }

  private def parseCombat(lines: Iterator[String]): (Combat, Iterator[String]) = {

    // find a valid entry, if one exists
    val buf = lines.buffered

    if (!buf.hasNext) {
      (null, buf)
    } else {
      val combat = new Combat
      val timeout = Config.inactivityThreshold * 1000

      do {
        if (waitForLine(buf, timeout)) {
          val action = parseAction(buf.next())
          combat.handle(action)
          afterActionCallback(combat)
        } else {
          if (combat.inCombat)
            combat.notifyIdle(timeout)
          else
            return (null, buf)
        }
      }
      while(!combat.ended)

      afterCombatCallback(combat)

      (combat, buf)
    }
  }

  // Delete the old log
  def delete() {

    // First, remove the old log
    try {
      val log = new File(logFilename)

      if (!log.delete())
        println("WARNING! Didn't delete log file: " + logFilename)

    } catch {
      case _: java.io.FileNotFoundException => ()
    }
  }

  // On-line processing of logs; does not store each combat as it is
  // parsed
  def parse() {

    // Start parsing...
    def parseForever(lines: Iterator[String]) {
      waitForLine(lines)
      val (_, pos) = parseCombat(lines)
      parseForever(pos)
    }

    while (true)
    {
      try {
        var lines = new OnlineIterator(logFilename, true)
        parseForever(lines)
      } catch {
        case _: java.io.FileNotFoundException => Thread.sleep(500)
      }
    }
  }

  // Off-line processing of logs
  def parseList(): List[Combat] = {
    val lines = new OnlineIterator(logFilename, false)

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

object Parser {
  class ParseError(val line: String) extends Throwable {
    override def toString = "Parse error while processing: " + line
  }
}
