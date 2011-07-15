import java.util.Date
import scala.collection.mutable.{HashMap, StringBuilder}
import scala.util.Sorting.quickSort

class Statistic(
  val name: String,
  val entity: Entity
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

    val header = "{ (* %s *) %.2f, {\n" format (name, full)
    val footer = "}}"

    val keys = bySecond.keys.toArray
    quickSort(keys)

    val body = for (t <- keys)
                 yield "{ " + ((t - entity.combat.start) / 1000) + ", " + bySecond(t) + "}"

    body.addString(builder, header, ", ", footer)
  }
}
