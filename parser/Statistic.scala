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

  def += (time: Date, amt: Double) {
    full += amt
    if (Config.makeGraphs && amt != 0)
      bySecond(time.getTime) += amt

    this
  }

  def += (pair: (Date, Double)) {
    this += (pair._1, pair._2)
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
