package scala.meta.internal.metals.codeactions

import org.eclipse.lsp4j
import org.eclipse.lsp4j.{CodeActionKind, CodeActionParams, RenameParams}

import scala.concurrent.{ExecutionContext, Future}
import scala.meta.{Defn, Tree, Type}
import scala.meta.internal.metals.{Buffers, CodeAction}
import scala.meta.internal.parsing.Trees
import scala.meta.pc.CancelToken
import org.eclipse.{lsp4j => l}

import scala.meta.internal.metals.MetalsEnrichments._
import scala.meta.internal.metals.codeactions.RenameActions.renameClassAsFileTitle
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
  ): Future[Seq[lsp4j.CodeAction]] = {
    val uri = params.getTextDocument.getUri
    val path = uri.toAbsolutePath
    val range = params.getRange

    Future.successful(trees.get(path) match {
      case Some(tree) =>
        val fileName = uri.toAbsolutePath.filename.replaceAll("\\.scala$", "")
        val classDefinitions = tree.getClassDefinitions

        val fileAndClassHaveDifferentName = classDefinitions.length == 1 &&
          classDefinitions.head.name.value != fileName &&
          classDefinitions.head.name.pos.toLSP.overlapsWith(range) &&
          !getExtendsNames(classDefinitions.head).contains(fileName)

        if (fileAndClassHaveDifferentName) List(renameClassAsFileAction())
        else Nil

      case _ => Nil
    })
  }

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
        range = range,
        classDefn = classDefn,
        fileName = fileName
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

  def resolve(data: RenameClassActionData, token: CancelToken)(implicit
      ec: ExecutionContext
  ): Future[l.CodeAction] = {

    val uri = data.uri
    val classDefn = data.classDefn
    val fileName = data.fileName

    renameClassAsFileResolveAction(uri, classDefn, fileName, token)

  }

  private def renameClassAsFileResolveAction(
      uri: String,
      classDefn: Defn.Class,
      fileName: String,
      token: CancelToken
  )(implicit
      ec: ExecutionContext
  ): Future[l.CodeAction] = {
    val className = classDefn.name.value

    val range = classDefn.name.pos.toLSP

    val textDocumentIdentifier: l.TextDocumentIdentifier =
      uri.toAbsolutePath.toTextDocumentIdentifier
    val namePosition: l.Position = range.getStart

    val renameParams = new RenameParams(
      textDocumentIdentifier,
      namePosition,
      fileName
    )

    for {
      workspaceEdit <- renameProvider.rename(renameParams, token)
      codeAction = {
        val codeAction = new l.CodeAction()

        codeAction.setTitle(
          RenameActions.renameClassAsFileTitle(fileName, className)
        )
        codeAction.setKind(l.CodeActionKind.Refactor)
        codeAction.setEdit(
          workspaceEdit
        )

        codeAction
      }
    } yield codeAction

  }
}

object RenameActions {
  def renameClassAsFileTitle(fileName: String, className: String) = {
    s"Rename class $className as $fileName"
  }
}
