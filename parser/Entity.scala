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

  def buildGraphData(builder: StringBuilder) = {
    val header = "{ \"%s\",\n" format name
    val footer = "}"

    val stats = List(damage, heals, damageTaken, healsTaken)
    val body = stats map ((stat: Statistic) => stat.graphData)

    body.addString(builder, header, ",\n", footer)
  }

  private def valPerSecond(thisVal: => Statistic, petVal: Entity => Double) =
    thisVal.full / combat.duration +
      (if (Config.combinePets) pets.map(petVal).sum else 0)

  def dps: Double = valPerSecond(damage, _.dps)
  def hps: Double = valPerSecond(heals, _.hps)
  def dtps: Double = valPerSecond(damageTaken, _.dtps)
  def htps: Double = valPerSecond(healsTaken, _.htps)

  def isPlayer: Boolean = id.t == Id.Type.Player

  def format(fmt: String): String = {
    val nameStr = ("%.3s" format name)
    val dpsStr = ("%.0f" format dps)
    val hpsStr = ("%.0f" format hps)
    val dtStr = ("%.0f" format damageTaken.full)

    fmt.replaceAll("%N", name)
       .replaceAll("%n", nameStr)
       .replaceAll("%d", dpsStr)
       .replaceAll("%h", hpsStr)
       .replaceAll("%D", dtStr)
  }

  override def toString: String = format(" %n:%d")
}
