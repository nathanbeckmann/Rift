import java.awt.Toolkit
import java.awt.datatransfer.Clipboard
import java.awt.datatransfer.StringSelection

object Clippy {

  private val clipboard = 
    try { Toolkit.getDefaultToolkit().getSystemClipboard() }
    catch { case _ => null }

  def copy(str: String) {
    if (clipboard != null) {
      val stringSelection = new StringSelection(str);
      clipboard.setContents(stringSelection, null);
    }
  }
}
