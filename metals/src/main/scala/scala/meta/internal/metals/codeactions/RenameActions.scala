package scala.meta.internal.metals.codeactions

import com.google.gson.JsonObject
import org.eclipse.lsp4j.{CodeActionKind, CodeActionParams, RenameParams}

import scala.concurrent.{ExecutionContext, Future}
import scala.meta.{Defn, Type}
import scala.meta.internal.metals.{Buffers, CodeAction}
import scala.meta.internal.parsing.Trees
import scala.meta.pc.CancelToken
import org.eclipse.{lsp4j => l}

import scala.meta.internal.metals.MetalsEnrichments._
import scala.meta.internal.rename.RenameProvider

class RenameActions(
    buffers: Buffers,
    trees: Trees,
    renameProvider: RenameProvider,
    resolveSupport: Boolean
) extends CodeAction {

  /**
   * This should be one of the String constants
   * listed in [[org.eclipse.lsp4j.CodeActionKind]]
   */
  override def kind: String = CodeActionKind.Refactor

  override def contribute(params: CodeActionParams, token: CancelToken)(implicit
      ec: ExecutionContext
  ): Future[Seq[l.CodeAction]] = {
    val uri = params.getTextDocument.getUri
    val path = uri.toAbsolutePath
    val range = params.getRange

    Future.successful(trees.get(path) match {
      case Some(tree) =>
        val fileName = fileNameFromUri(uri)
        val classDefinitions = tree.getClassDefinitions

        val fileAndClassHaveDifferentName = classDefinitions.length == 1 &&
          classDefinitions.head.name.value != fileName &&
          classDefinitions.head.name.pos.toLSP.overlapsWith(range) &&
          !getExtendsNames(classDefinitions.head).contains(fileName)

        if (fileAndClassHaveDifferentName)
          List(renameClassAsFileAction(uri, classDefinitions.head, fileName))
        else Nil

      case _ => Nil
    })
  }

  def fileNameFromUri(uri: String) =
    uri.toAbsolutePath.filename.replaceAll("\\.scala$", "")

  private def renameClassAsFileAction(
      uri: String,
      classDefn: Defn.Class,
      fileName: String
  ): l.CodeAction = {
    val className = classDefn.name.value

    val range = classDefn.name.pos.toLSP

    val codeAction = new l.CodeAction()

    codeAction.setTitle(
      RenameActions.renameClassAsFileTitle(fileName, className)
    )
    codeAction.setKind(l.CodeActionKind.Refactor)
    codeAction.setData(
      RenameClassActionData(
        uri = uri,
        range = range
      )
    )

    codeAction
  }

  private def getExtendsNames(classDefn: Defn.Class): List[String] =
    classDefn.templ.inits.flatMap {
      _.tpe match {
        case Type.Name(value) => Some(value)
        case _ => None
      }
    }

  override def resolve(token: CancelToken)(implicit
      ec: ExecutionContext
  ): PartialFunction[JsonObject, Future[l.CodeAction]] = {
    case CodeActionDataParsers.renameClassActionDataParser.Jsonized(
          RenameClassActionData(uri, range, RenameActions.codeActionDataType)
        ) =>
      renameClassAsFileResolveAction(uri, range, token)

  }

  private def renameClassAsFileResolveAction(
      uri: String,
      range: l.Range,
      token: CancelToken
  )(implicit
      ec: ExecutionContext
  ): Future[l.CodeAction] = {

    val fileName = fileNameFromUri(uri)

    val textDocumentIdentifier: l.TextDocumentIdentifier =
      uri.toAbsolutePath.toTextDocumentIdentifier

    range.getStart.setCharacter(range.getStart.getCharacter + 1)

    val namePosition: l.Position = range.getStart

    val renameParams = new RenameParams(
      textDocumentIdentifier,
      namePosition,
      fileName
    )
    pprint.log(renameParams)

    for {
      workspaceEdit <- renameProvider.rename(renameParams, token)
      codeAction = {
        val codeAction = new l.CodeAction()

        codeAction.setTitle(
          s"Resolve Rename Class as $fileName"
        )
        codeAction.setKind(l.CodeActionKind.Refactor)
        codeAction.setEdit(
          workspaceEdit
        )

        pprint.log(codeAction)
        codeAction
      }
    } yield codeAction

  }
}

object RenameActions {
  val codeActionDataType: String = "RenameClassActionData"

  def renameClassAsFileTitle(fileName: String, className: String) = {
    s"Rename class $className as $fileName"
  }
}
