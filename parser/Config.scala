object Config {

  val saveActions = false
  val onlyPlayers = true
  val combinePets = true
  var makeGraphs = false
  val trackAbilities = false
  val includeAbsorbed = true
  val healsAreCombatActions = false

  // The number of seconds to wait with no damaging spells before
  // considering that combat has ended
  val inactivityThreshold = 5

  // The number of seconds combat must last before we process it.
  val minimumCombatLength = 30

  val ignoredActions = List("Shocking Cipher", "Deathly Flames", "Sourcestone Annihilation")

}
