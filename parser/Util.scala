import java.util.Calendar
import java.text.SimpleDateFormat

object Util {

  private val now_format = "yyyy-MM-dd-HH:mm:ss";

  def now: String = {
    val cal = Calendar.getInstance()
    val sdf = new SimpleDateFormat(now_format)

    sdf.format(cal.getTime())
  }

}
