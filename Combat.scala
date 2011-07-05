import java.util.Date

class Entity {

  def name: String
  def combat: Combat
  def actions: List[Entry]

  def damage: Double
  def heals: Double
  def damageTaken: Double
  def healsTaken: Double

  def dps: Double = damage / combat.timeElapsed
  def hps: Double = heals / combat.timeElapsed
  def dtps: Double = damageTaken / combat.timeElapsed
  def htps: Double = healsTaken / combat.timeElapsed
}

class Combat {
  def entities: List[Entity]

  def start: Date
  def end: Date
  def timeElapsed: Double
}
