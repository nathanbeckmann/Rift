import java.util.Date
import scala.collection.mutable.{DoubleLinkedList, Set, StringBuilder}

class Entity(
  val id: Id,
  val name: String,
  val combat: Combat) extends Grapher {

  val actions: DoubleLinkedList[Action] = new DoubleLinkedList[Action]()
  val pets: Set[Entity] = Set[Entity]()

  var damage = new Statistic("damage", this)
  var heals = new Statistic("heals", this)
  var damageTaken = new Statistic("damage taken", this)
  var healsTaken = new Statistic("heals taken", this)

  def buildGraphData(builder: StringBuilder) = {
    val header = "{ \"%s\",\n" format name
    val footer = "}"

    val stats = List(damage, heals, damageTaken, healsTaken)
    val body = stats map ((stat: Statistic) => stat.graphData)

    body.addString(builder, header, ",\n", footer)
  }

  private def valPerSecond(thisVal: => Statistic, petVal: Entity => Double) =
    1000 * thisVal.full / combat.duration +
      (if (Config.combinePets) pets.map(petVal).sum else 0)

  def dps: Double = valPerSecond(damage, _.dps)
  def hps: Double = valPerSecond(heals, _.hps)
  def dtps: Double = valPerSecond(damageTaken, _.dtps)
  def htps: Double = valPerSecond(healsTaken, _.htps)

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
