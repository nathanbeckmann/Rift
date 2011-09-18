import java.util.Calendar
import java.text.SimpleDateFormat

object Util {

  private val now_format = "yyyy-MM-dd"; //-HH:mm:ss";

  def now: String = {
    val cal = Calendar.getInstance()
    val sdf = new SimpleDateFormat(now_format)

    sdf.format(cal.getTime())
  }

  def format(num: Double): String = {
    if (num > 1e6)
      "%.3gM" format (num / 1e6)
    else if (num > 1e4)
      "%.3gk" format (num / 1e3)
    else
      "%.0f" format num
  }

}
