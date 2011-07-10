import scala.collection.mutable.DoubleLinkedList
import scala.collection.mutable.Set

class Entity(
  val id: Id,
  val name: String,
  val combat: Combat) {

  val actions: DoubleLinkedList[Action] = new DoubleLinkedList[Action]()
  val pets: Set[Entity] = Set[Entity]()

  var damage: Double = 0
  var heals: Double = 0
  var damageTaken: Double = 0
  var healsTaken: Double = 0

  private def valPerSecond(thisVal: => Double, petVal: Entity => Double) =
    1000 * thisVal / combat.duration + (if (Config.combinePets) pets.map(petVal).sum else 0)

  def dps: Double = valPerSecond(damage, _.dps)
  def hps: Double = valPerSecond(heals, _.hps)
  def dtps: Double = valPerSecond(damageTaken, _.dtps)
  def htps: Double = valPerSecond(healsTaken, _.htps)

  def format(fmt: String): String = {
    val nameStr = ("%.3s" format name)
    val dpsStr = ("%.0f" format dps)
    val hpsStr = ("%.0f" format hps)
    val dtStr = ("%.0f" format damageTaken)
    fmt.replaceAll("%N", name)
       .replaceAll("%n", nameStr)
       .replaceAll("%d", dpsStr)
       .replaceAll("%h", hpsStr)
       .replaceAll("%D", dtStr)
  }

  override def toString: String = format(" %n:%d")
}
