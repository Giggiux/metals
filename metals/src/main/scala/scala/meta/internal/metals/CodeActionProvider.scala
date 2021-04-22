package scala.meta.internal.metals

import com.google.gson.JsonObject
import org.eclipse.lsp4j.CodeActionResolveSupportCapabilities

import scala.collection.JavaConverters._
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.meta.internal.metals.codeactions._
import scala.meta.internal.parsing.Trees
import scala.meta.pc.CancelToken
import org.eclipse.{lsp4j => l}

import scala.meta.internal.metals.JsonParser.{
  XtensionSerializableToJson,
  XtensionSerializedAsJson
}
import scala.meta.internal.rename.RenameProvider

final class CodeActionProvider(
    codeActionCapabilities: Option[
      l.CodeActionCapabilities
    ],
    compilers: Compilers,
    buffers: Buffers,
    buildTargets: BuildTargets,
    scalafixProvider: ScalafixProvider,
    renameProvider: RenameProvider,
    trees: Trees
)(implicit ec: ExecutionContext) {

  private val codeActionResolveSupportCapabilities
      : Option[CodeActionResolveSupportCapabilities] =
    codeActionCapabilities.map(_.getResolveSupport)

  private val codeActionDataSupport: Option[Boolean] =
    codeActionCapabilities.map(_.getDataSupport)

  private val resolveSupport: Boolean = (for {
    resolveSupportCapabilities <- codeActionResolveSupportCapabilities
    if (resolveSupportCapabilities != null)
    dataSupport <- codeActionDataSupport
    properties = resolveSupportCapabilities.getProperties
  } yield dataSupport && properties.contains("edit")).getOrElse(false)

  private val allActions: List[CodeAction] = List(
    new ImplementAbstractMembers(compilers),
    new ImportMissingSymbol(compilers),
    new CreateNewSymbol(),
    new StringActions(buffers, trees),
    new OrganizeImports(scalafixProvider, buildTargets),
    new InsertInferredType(trees, compilers),
    new RenameActions(buffers, trees, renameProvider, resolveSupport)
  )

  def codeActions(
      params: l.CodeActionParams,
      token: CancelToken
  )(implicit ec: ExecutionContext): Future[Seq[l.CodeAction]] = {
    def isRequestedKind(action: CodeAction): Boolean =
      Option(params.getContext.getOnly) match {
        case Some(only) =>
          only.asScala.toSet.exists(requestedKind =>
            action.kind.startsWith(requestedKind)
          )
        case None => true
      }

    val actions = allActions.collect {
      case action if isRequestedKind(action) => action.contribute(params, token)
    }

    Future.sequence(actions).map(_.flatten)
  }

  def resolve(
      codeAction: l.CodeAction,
      token: CancelToken
  )(implicit ec: ExecutionContext): Future[l.CodeAction] = {

    lazy val identityCodeAction = Future.successful(codeAction)

    val data = codeAction.getData.toJsonObject

    val basePF: PartialFunction[JsonObject, Future[l.CodeAction]] = { case _ =>
      identityCodeAction
    }

    val partialFunction: PartialFunction[JsonObject, Future[l.CodeAction]] =
      allActions.foldRight(basePF)((codeAction, pf) =>
        codeAction.resolve(token) orElse pf
      )

    partialFunction(data)

  }

}
