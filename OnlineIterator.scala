import java.io.{File,FileInputStream,InputStreamReader,BufferedReader}

class OnlineIterator(file: String) extends Iterator[String] {
  private val stream = new FileInputStream(file)
  private val reader = new BufferedReader(new InputStreamReader(stream))

  def hasNext = reader.ready
  def next = reader.readLine
}
