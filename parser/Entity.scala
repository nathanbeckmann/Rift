import java.util.Date
import scala.collection.mutable.HashMap
import scala.collection.mutable.MapLike
import scala.collection.mutable.DoubleLinkedList
import scala.collection.mutable.Set
import scala.util.Sorting.stableSort

class Entity(
  val id: Id,
  val name: String,
  val combat: Combat) {

  val actions: DoubleLinkedList[Action] = new DoubleLinkedList[Action]()
  val pets: Set[Entity] = Set[Entity]()

  class Statistic {
    var full: Double = 0

    val bySecond = new HashMap[Date, Double] {
      override def default(key: Date): Double = 0
    }

    def += (time: Date, amt: Double) {
      full += amt
      if (Config.makeGraphs)
        bySecond(time) += amt
      this
    }

    def += (pair: (Date, Double)) {
      this += (pair._1, pair._2)
    }

  }

  var damage = new Statistic
  var heals = new Statistic
  var damageTaken = new Statistic
  var healsTaken = new Statistic

  def writeGraphData(w: java.io.Writer) {
    w write ("%s = {" format name)

    val stats = List(damage, heals, damageTaken, healsTaken)

    val keys = damage.bySecond.keys.toArray
    stableSort(keys, (k1: Date, k2: Date) => k1.getTime < k2.getTime)

    val data = for (t <- keys)
                 yield "{ " + (t getTime) + ", {" + (stats.map(_.bySecond(t)) mkString ", ") + "} }"

    w write (data mkString ",\n")

    w write "}\n\n"
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
