import scala.collection.mutable.DoubleLinkedList
import scala.collection.mutable.Map
import scala.util.Sorting.stableSort

class Combat {
  def start: Long = actions.head.time.getTime
  def end: Long = actions.last.time.getTime
  def duration: Long = end - start

  var actions: DoubleLinkedList[Action] = new DoubleLinkedList[Action]()
  var inCombat: Boolean = false

  val entities = Map[String, Entity]()

  def handle(action: Action) {

    actions = actions :+ action

    // extending Map to use default didn't work for some reason,
    // probably I'm a nub and a moron
    if (!(entities contains action.source)) entities += action.source -> new Entity(action.source, this)
    if (!(entities contains action.target)) entities += action.target -> new Entity(action.target, this)

    val source = entities(action.source)
    val target = entities(action.target)
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

  override def toString: String = {
    val dpsers = entities.values.filter(_.dps > 0).toArray
    val secs = duration / 1000
    val mins = secs / 60
    stableSort[Entity](dpsers, (_: Entity).dps > (_: Entity).dps)
    mins + ":" + (secs % 60) + dpsers.map(_.toString).reduce(_ + _)
  }
}
