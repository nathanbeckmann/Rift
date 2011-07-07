import scala.collection.mutable.DoubleLinkedList

class Entity(
  val name: String,
  val combat: Combat) {

  val actions: DoubleLinkedList[Action] = new DoubleLinkedList[Action]()

  var damage: Double = 0
  var heals: Double = 0
  var damageTaken: Double = 0
  var healsTaken: Double = 0

  def dps: Double = 1000 * damage / combat.duration
  def hps: Double = 1000 * heals / combat.duration
  def dtps: Double = 1000 * damageTaken / combat.duration
  def htps: Double = 1000 * healsTaken / combat.duration

  def format(fmt: String): String = {
    val nameStr = ("%.3s" format name)
    val dpsStr = ("%.0f" format dps)
    val hpsStr = ("%.0f" format hps)
    fmt replaceAll("%n", nameStr) replaceAll("%d", dpsStr) replaceAll("%h", hpsStr)
  }

  override def toString: String = format("|%n:%d")
}
