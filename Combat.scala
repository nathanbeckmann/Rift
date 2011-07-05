import scala.collection.mutable.DoubleLinkedList
import scala.collection.mutable.Map

class Combat {
  def start: Long = actions.head.time.getTime
  def end: Long = actions.last.time.getTime
  def duration: Long = end - start

  var actions: DoubleLinkedList[Action] = new DoubleLinkedList[Action]()

  val entities = Map[String, Entity]()
}
