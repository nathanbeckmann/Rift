import scala.collection.mutable.DoubleLinkedList
import scala.collection.mutable.Map
import scala.util.Sorting.stableSort
import scala.util.matching.Regex

class Combat {
  def start: Long = actions.head.time.getTime
  def end: Long = actions.last.time.getTime
  def duration: Long = end - start

  var actions: DoubleLinkedList[Action] = new DoubleLinkedList[Action]()
  var inCombat: Boolean = false

  val entities = Map[String, Entity]()

  private val saveActions = false

  def handle(action: Action) {

    if (saveActions || action.isBookend)
      actions = actions :+ action

    // extending Map to use default didn't work for some reason,
    // probably I'm a nub and a moron
    if (!(entities contains action.source)) entities += action.source -> new Entity(action.source, this)
    if (!(entities contains action.target)) entities += action.target -> new Entity(action.target, this)

    val source = entities(action.source)
    val target = entities(action.target)

    if (saveActions || action.isBookend)
      source.actions :+ action

    if (action.isDmg) {
      source.damage += action.amount
      target.damageTaken += action.amount
    }
    else if (action.isHeal) {
      source.heals += action.amount
      target.healsTaken += action.amount
    }

    action.name match {
      case "Combat Begin" => inCombat = true
      case "Combat End" => inCombat = false
      case _ =>
    }
  }

  // Pass in a string of with these formatting rules:
  //  %t - combat time
  //  %<num>d - <num> top dpsers
  //  %<num>h - <num> top hpsers
  def format(fmt: String): String = {

    val secs = duration / 1000
    val mins = secs / 60
  
    val timeStr = mins + ":" + (secs % 60)

    def top(query: Entity => Double, entityFmt: String, num: Int): String = {
      val list = entities.values.filter(query(_) > 0).toArray
      stableSort[Entity](list, query(_: Entity) > query(_: Entity))
      list.take(num).map(_ format entityFmt) mkString ""
    }

    // example: replaceTop(fmt, "%d", _.dps, " %n:%d")
    def replaceTop(src: String, queryTerm: String, query: Entity => Double, entityFmt: String) = { 
      val matcherStr = "(?s).*" + (queryTerm replace ("%", "%(\\d+)")) + ".*"
      val matcher = matcherStr.r

      try {
        val matcher(num) = src
        val str = top(query, entityFmt, num.toInt)
	val replaceRegex = queryTerm replaceAll ("%", """%(\\d+)""")

        src replaceAll (replaceRegex, str)
      } catch {
        case _: MatchError => println("Couldn't match: " + matcherStr); src
      }
    }

    // this is ugly -- use implicit conversions to chain these calls?
    replaceTop(                        // replace hps
      replaceTop(                      // replace dps
        fmt replaceAll("%t", timeStr), // replace time
        "%d", _.dps, " %n:%d"),
      "%h", _.hps, " %n:%h")
  }

  override def toString: String = format("%t| DPS%10d| HPS%5h")
}
