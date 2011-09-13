import scala.collection.mutable.{Buffer, Map, StringBuilder}
import scala.util.Sorting.stableSort
import scala.util.matching.Regex

class Combat extends Grapher {

  var start: Long = 0
  var end: Long = 0
  private var last: Long = 0            // the last action, regardless
                                        // of type. see comment below
                                        // for inCombat

  def duration: Long = (end - start) / 1000
  private def idle: Long = (last - end) / 1000
  
  val actions: Buffer[Action] = Buffer.empty[Action]

  private var deferredActions: Buffer[Action] = Buffer.empty[Action]

  val entities = Map[Id, Entity]()

  // The timing model for combat is from the first to last damaging
  // action. Combat is reset whenever no damaging action occurs within
  // Config.inactivityThreshold seconds
  def inCombat = (start != 0) && (idle <= Config.inactivityThreshold)
  def ended = (start != 0) && (idle > Config.inactivityThreshold)

  private def isCombatAction(action: Action) = action.isDmg || (Config.healsAreCombatActions && action.isHeal)

  private def isIgnoredAction(action: Action) =
    (Config.ignoredActions contains action.name) ||
    action.isBookend ||
    (action.source.r == Id.Relation.Other
     && action.target.r == Id.Relation.Other
     && action.owner.r == Id.Relation.Other)

  private def updateTimers(action: Action) {
    if (isCombatAction(action)) {
      if (start == 0)
        start = action.time.getTime

      end = action.time.getTime
    }

    last = action.time.getTime
  }

  // Deferred processing of an action
  private def process(action: Action) {
      // extending Map to use default didn't work for some reason,
      // probably I'm a nub and a moron
    if (!(entities contains action.source))
      entities += action.source -> new Entity(action.source, action.sourceName, this)
    if (!(entities contains action.target))
      entities += action.target -> new Entity(action.target, action.targetName, this)
    
    val source = entities(action.source)
    val target = entities(action.target)
    
    // pets
    (entities get action.owner) match {
      case None => ()
        case Some(owner) => {
          if (owner.id.t != Id.Type.Unknown)
            owner.pets += source        // pets is a set; no duplicates
        }
    }
    
    if (Config.saveActions)
      source.actions :+ action
    
    if (action.isDmg) {
      source.damage += action
      target.damageTaken += action
    }
    else if (action.isHeal) {
      source.heals += action
      target.healsTaken += action
    }
  }

  // Public interface to process a single action
  def handle(action: Action) {
    if (Config.saveActions)
      actions :+ action

    if (isIgnoredAction(action))
      return
  
    updateTimers(action)

    // we defer processing of actions until we know that they should
    // be included in the stats of this combat (see updateTimers)
    if (inCombat) {
      deferredActions += action
    }

    if (isCombatAction(action)) {
      deferredActions.foreach(process)
      deferredActions.clear()
    }
  }

  // Notify this structure that nothing has happened for a while,
  // possibly end combat
  // 
  // The milliseconds parameter is in real-time -- not rift time. (For
  // offline parsing these are not the same.)
  def notifyIdle(milliseconds: Long) {
    last += milliseconds
  }

  // Pass in a string of with these formatting rules:
  //  %t - combat time
  //  %<num>d - <num> top dpsers
  //  %<num>h - <num> top hpsers
  def format(fmt: String): String = {

    val secs = duration
    val mins = secs / 60
  
    val timeStr = mins + ":" + ("%02d" format (secs % 60))

    // Get the top <num> entities for a particular query
    def top(query: Entity => Double, entityFmt: String,
            num: Int, onlyPlayers: Boolean): String = {

      // First find valid entities
      val valid = entities.filter(entry => if (onlyPlayers) entry._1.t == Id.Type.Player else true)

      val sum = valid.values.map(query).reduce(_ + _)

      // Now only those with a positive value for the query
      val list = valid.values.filter(query(_) > 0).toArray

      stableSort[Entity](list, query(_: Entity) > query(_: Entity))

      val top = list.take(num)
      val topStr = top.map(_ format entityFmt) mkString ""

      " " + ("%.0f" format sum) + topStr
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
      (str: String) => replaceTop(str, "%D", _.damageTaken.full, " %N:%D", false), // replace dt  
      (str: String) => replaceTop(str, "%h", _.hps, " %n:%h"),                // replace hps 
      (str: String) => replaceTop(str, "%d", _.dps, " %n:%d"),                // replace dps 
      (str: String) => str replaceAll("%t", timeStr))                         // replace time
    .foldLeft(fmt)((str, f) => f(str))
  }

  def buildGraphData(builder: StringBuilder) = {

    val (_, maxdmg) = entities max Ordering.by((_ : (_, Entity))._2.damageTaken.full)

    val header = "headline = \"%s : %.2f\";\ndata = { \n" format (maxdmg.name, maxdmg.damageTaken.full)
    val footer = "};\n"

    val body = entities.values.filter(_.id.t == Id.Type.Player) map ((ent: Entity) => ent.graphData)

    body.addString(builder, header, ",\n\n", footer)
  }

  // Default string formatting
  override def toString: String = format("%t%1D -DPS-%10d -HPS-%5h")
}
