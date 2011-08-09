import java.io.{File,FileInputStream,InputStreamReader,BufferedReader}
import scala.collection.mutable.StringBuilder

class OnlineIterator(file: String, skip: Boolean) extends Iterator[String] {
  private val stream = new FileInputStream(file)

  private def skipper() {
    val available = stream.available
    if (available > 0) {
      assert(stream.skip(available) == available)
      skipper()
    }
  }
  if (skip) skipper()

  private val reader = new BufferedReader(new InputStreamReader(stream))

  private val sb = new StringBuilder

  private def ready: Boolean = sb.nonEmpty && sb.last == '\n'

  def hasNext: Boolean = {
    if (ready)
      true
    else {
      while (!ready && reader.ready) {
        val character = reader.read().toChar
        if (character != '\r')
          sb += character
      }
      ready
    }
  }

  def next: String = {
    if (ready) {
      val result = sb.init.mkString
      sb.clear()
      result
    } else {
      null
    }
  }
}
