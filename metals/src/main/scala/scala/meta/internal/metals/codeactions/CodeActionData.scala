package scala.meta.internal.metals.codeactions

import scala.meta.Defn
import org.eclipse.{lsp4j => l}

trait CodeActionData {
  val uri: String
  val range: l.Range
}
case class RenameClassActionData(
    val uri: String,
    val range: l.Range,
    val classDefn: Defn.Class,
    val fileName: String
) extends CodeActionData
