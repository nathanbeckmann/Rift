import scala.util.matching.Regex
import scala.collection.mutable.Map

class Id(
  val t: Id.Type.Type,
  val id: String) {

  override def hashCode = id.hashCode
  override def equals(that: Any) =
    if (!t.isInstanceOf[Id]) {
      val that2 = that.asInstanceOf[Id]
      id == that2.id
    } else
      false

  override def toString = t + ":" + id
}

object Id {

  object Type extends Enumeration {

    type Type = Value
    val Unknown, Player, Nonplayer = Value

    class Error(val c: Char) extends Throwable

    def apply(s: String): Type = apply(s charAt 0)
    def apply(c: Char): Type = c match {
      case 'P' => Player
      case 'N' => Nonplayer
      case 'X' => Unknown
      case _ => throw new Error(c)
    }
  }
  import Type.{Type, Unknown, Player, Nonplayer}

  private val nothing = new Id(Unknown, "")
  private val ids = Map[String, Id]("T=X#R=X#0" -> nothing)
  private val parseRegex = """T=([XPN])#R=.#(\d+)""".r

  def apply() = nothing

  // Cache IDs to avoid thrashing memory
  def apply(str: String) =
    (ids get str) match {
      case Some(id) => id
      case None => str match {
        case parseRegex(c, id) =>
          try {
            val newId = new Id(Type(c), id)
            ids += str -> newId
            newId
          }
          catch { case _ => throw new Parser.ParseError(str) }
        case e: Any => { println(e); throw new Parser.ParseError(str) }
      }
    }
}
