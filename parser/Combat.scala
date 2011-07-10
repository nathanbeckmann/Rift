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

  val entities = Map[Id, Entity]()

  def handle(action: Action) {

    if (Config.saveActions || action.isBookend)
      actions = actions :+ action

    // No more processing needed for bookends
    if (!action.isBookend) {
  
      // extending Map to use default didn't work for some reason,
      // probably I'm a nub and a moron
      if (!(entities contains action.source))
        entities += action.source -> new Entity(action.source, action.sourceName, this)
      if (!(entities contains action.target))
        entities += action.target -> new Entity(action.target, action.targetName, this)
    
      val source = entities(action.source)
      val target = entities(action.target)
    
      // Add pet to owner; pets is a set so this won't produce duplicates
      (entities get action.owner) match {
        case None => ()
        case Some(owner) => {
          if (owner.id.t != Id.Type.Unknown)
            owner.pets += source
        }
      }
    
      if (Config.saveActions)
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
  
    val timeStr = mins + ":" + ("%02d" format (secs % 60))

    // Get the top <num> entities for a particular query
    def top(query: Entity => Double, entityFmt: String,
            num: Int, onlyPlayers: Boolean): String = {

      // First find valid entities
      val valid = entities.filter(entry => if (onlyPlayers) entry._1.t == Id.Type.Player else true)

      // Now only those with a positive value for the query
      val list = valid.values.filter(query(_) > 0).toArray

      stableSort[Entity](list, query(_: Entity) > query(_: Entity))

      list.take(num).map(_ format entityFmt) mkString ""
    }

    // Put the top <num> entities into a string somewhere
    // example: replaceTop(fmt, "%d", _.dps, " %n:%d")
    def replaceTop(src: String, queryTerm: String, query: Entity => Double,
                   entityFmt: String, onlyPlayers: Boolean = Config.onlyPlayers) = { 
      val matcherStr = "(?s).*" + (queryTerm replace ("%", "%(\\d+)")) + ".*"
      val matcher = matcherStr.r

      try {
        val matcher(num) = src
        val str = top(query, entityFmt, num.toInt, onlyPlayers)
	val replaceRegex = queryTerm replaceAll ("%", """%(\\d+)""")

        src replaceAll (replaceRegex, str)
      } catch {
        case _: MatchError => src
      }
    }

    // chain replacement operations to produce final formatted string
    List(
      (str: String) => replaceTop(str, "%D", _.damageTaken, " %N:%D", false), // replace dt  
      (str: String) => replaceTop(str, "%h", _.hps, " %n:%h"),                // replace hps 
      (str: String) => replaceTop(str, "%d", _.dps, " %n:%d"),                // replace dps 
      (str: String) => str replaceAll("%t", timeStr))                         // replace time
    .foldLeft(fmt)((str, f) => f(str))
  }

  // Default string formatting
  override def toString: String = format("%t%1D -DPS-%10d -HPS-%5h")
}
