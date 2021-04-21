package scala.meta.internal.metals.codeactions

import org.eclipse.{lsp4j => l}

import scala.meta.internal.metals.JsonParser

trait CodeActionData {
  val uri: String
  val range: l.Range
  val actionType: String
}
case class RenameClassActionData(
    uri: String,
    range: l.Range,
    actionType: String = RenameActions.codeActionDataType
) extends CodeActionData

object CodeActionDataParsers {
  val renameClassActionDataParser = new JsonParser.Of[RenameClassActionData]
}
