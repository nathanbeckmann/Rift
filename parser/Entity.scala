import java.util.Date
import scala.collection.mutable.{Buffer, Set, StringBuilder}

class Entity(
  val id: Id,
  val name: String,
  val combat: Combat) extends Grapher {

  val actions: Buffer[Action] = Buffer.empty[Action]
  val pets: Set[Entity] = Set[Entity]()

  var damage: Statistic = new Statistic("damage", this, (ent: Entity) => ent.damage)
  var heals: Statistic = new Statistic("heals", this, (ent: Entity) => ent.heals)
  var damageTaken: Statistic = new Statistic("damage taken", this, (ent: Entity) => ent.damageTaken)
  var healsTaken: Statistic = new Statistic("heals taken", this, (ent: Entity) => ent.healsTaken)

  var activeTime: Long = 0
  private var lastAction: Long = 0

  def buildGraphData(builder: StringBuilder) = {
    val header = "{ \"%s\",\n" format name
    val footer = "}"

    val stats = List(damage, heals, damageTaken, healsTaken)
    val body = stats map ((stat: Statistic) => stat.graphData)

    body.addString(builder, header, ",\n", footer)
  }

  private def valPerSecond(stat: Entity => Statistic, time: Long) = {
    val selfStat = stat(this).full
    val petStat = if (Config.combinePets) {
      pets.map(e => stat(e).full).sum
    } else {
      0
    }
    (selfStat + petStat) / time
  }

  def dps: Double = valPerSecond(_.damage, combat.duration)
  def hps: Double = valPerSecond(_.heals, combat.duration)
  def dtps: Double = damageTaken.full / combat.duration
  def htps: Double = healsTaken.full / combat.duration

  def adps: Double = valPerSecond(_.damage, activeTime)
  def ahps: Double = valPerSecond(_.heals, activeTime)
  def adtps: Double = damageTaken.full / activeTime
  def ahtps: Double = healsTaken.full / activeTime

  def isPlayer: Boolean = id.t == Id.Type.Player

  def format(fmt: String): String = {
    val nameLongStr = "%.10s" format name
    val nameStr = "%.3s" format name
    val dpsStr = Util.format(dps)
    val hpsStr = Util.format(hps)
    val dtStr = Util.format(damageTaken.full)
    val dtpsStr = Util.format(dtps)
    val htpsStr = Util.format(htps)
    val adpsStr = Util.format(adps)
    val ahpsStr = Util.format(ahps)
    val adtpsStr = Util.format(adtps)
    val ahtpsStr = Util.format(ahtps)
    val activePercentageStr = "%d" format (100 * activeTime / combat.duration)

    fmt.replaceAll("%N", nameLongStr)
       .replaceAll("%n", nameStr)
       .replaceAll("%dt", dtpsStr)
       .replaceAll("%ht", htpsStr)
       .replaceAll("%d", dpsStr)
       .replaceAll("%h", hpsStr)
       .replaceAll("%D", dtStr)
       .replaceAll("%adt", adtpsStr)
       .replaceAll("%aht", ahtpsStr)
       .replaceAll("%ad", adpsStr)
       .replaceAll("%ah", ahpsStr)
       .replaceAll("%p", activePercentageStr)
  }

  private def activate(time: Long) {
    if (lastAction != 0 ||
        time - lastAction <= Config.inactivityThreshold) {
      activeTime += (time - lastAction) / 1000
    }
    lastAction = time
  }

  // called when an the entity performs an action
  def act(action: Action) {
    if (Config.saveActions)
      actions :+ action
    
    if (action.isDmg) {
      damage += action
    }
    else if (action.isHeal) {
      heals += action
    }

    if (action.isDmg || action.isHeal) {
      activate(action.time.getTime)
    }
  }

  // called when an entity receives some action
  def receive(action: Action) {
    if (action.isDmg) {
      damageTaken += action
    }
    else if (action.isHeal) {
      healsTaken += action
    }
  }

  override def toString: String = format(" %n:%d")
}
