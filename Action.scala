import java.util.Date

class Action (
  val time: Date,
  val source: String,
  val target: String,
  val name: String,
  val amount: Double,
  val category: Int) {

  object Categories {
    val CAST_START = 1
    val HIT = 3
    val DOT = 4
    val HEAL = 5
    val BUFF_START = 6
    val BUFF_END = 7
    val DEBUFF_START = 8
    val DEBUFF_END = 9
    val MISS = 10
    val SLAIN = 11
    val DODGE = 15
    val CRITICAL_HIT = 23
    val IMMUNE = 26
    val REGEN = 27                        // ??
    val CRITICAL_HEAL = 28
  }
  import Categories._

  def isDmg: Boolean = category match {
    case HIT => true
    case DOT => true
    case CRITICAL_HIT => true
    case _ => false
  }

  def isHeal: Boolean = category match {
    case HEAL => true
    case CRITICAL_HEAL => true
    case _ => false
  }
}
