class Action(
  val source: String,
  val target: String,
  val name: String,
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
}

// An action that doesn't directly change HP of something; debuff, etc
class MetaAction(name: String) extends Action

class CombatStart extends MetaAction("combat")
class CombatEnd extends MetaAction("combat")

class BuffStart(name: String) extends MetaAction(name)
class BuffEnd(name: String) extends MetaAction(name)

class DebuffStart(name: String) extends MetaAction(name)
class DebuffEnd(name: String) extends MetaAction(name)

// An action that deals damage or heals
class 

class Heal extends Action {
  def am
}
