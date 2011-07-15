trait Grapher {
  def buildGraphData(builder: StringBuilder): Unit

  def graphData: String = {
    val builder = new StringBuilder()
    buildGraphData(builder)
    builder.toString
  }
}
