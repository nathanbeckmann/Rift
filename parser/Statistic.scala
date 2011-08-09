import java.util.Date
import scala.collection.mutable.{HashMap, StringBuilder}
import scala.util.Sorting.quickSort

class Statistic(
  val name: String,
  val entity: Entity,
  childAccessor: Entity => Statistic
) extends Grapher {
  var full: Double = 0

  val bySecond = new HashMap[Long, Double] {
    override def default(key: Long): Double = 0
  }

  val byName = new HashMap[String, Double] {
    override def default(key: String): Double = 0
  }

  def += (action: Action) {
    full += action.amount
    if (action.amount != 0) {
      if (Config.makeGraphs)
        bySecond(action.time.getTime) += action.amount
      if (Config.trackAbilities)
        byName(action.name) += action.amount
    }

    this
  }

  def buildGraphData(builder: StringBuilder) {

    val petStatFull = 
      if (entity.pets nonEmpty)
        entity.pets map(childAccessor) map(_.full) reduce(_+_)
      else
        0

    val header = "{ (* %s *) %.2f, {\n" format (name, full + petStatFull)
    val footer = "}}"

    val keys = bySecond.keys.toArray
    quickSort(keys)

    val body = for (t <- keys;
                    val petStat =
                      if (entity.pets nonEmpty)
                        entity.pets map(childAccessor) map(_.bySecond(t)) reduce(_+_)
                      else
                        0;
                    val stat = bySecond(t) + petStat)
                 yield "{ " + ((t - entity.combat.start) / 1000) + ", " + stat + "}"

    body.addString(builder, header, ", ", footer)
  }
}
