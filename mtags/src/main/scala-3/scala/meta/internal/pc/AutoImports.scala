package scala.meta.internal.pc

import scala.annotation.tailrec
import scala.jdk.CollectionConverters._

import scala.meta.internal.mtags.MtagsEnrichments._
import scala.meta.internal.pc.AutoImports.AutoImportEdits
import scala.meta.pc.PresentationCompilerConfig

import dotty.tools.dotc.ast.tpd._
import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.core.Denotations._
import dotty.tools.dotc.core.Flags._
import dotty.tools.dotc.core.Names._
import dotty.tools.dotc.core.Symbols._
import dotty.tools.dotc.interactive.Interactive
import dotty.tools.dotc.util.SourcePosition
import dotty.tools.dotc.util.Spans
import org.eclipse.{lsp4j => l}

object AutoImports {

  enum AutoImport {

    /**
     * Trivial import: `Future -> (Future, import scala.concurrent.Future)`
     */
    case Simple(sym: Symbol)

    /**
     * Rename symbol owner and add renamed prefix to tpe symbol
     * `Map -> (ju.Map, import java.{util => ju})`
     */
    case Renamed(sym: Symbol, ownerRename: String)

    /**
     *  Import owner and add prefix to tpe symbol
     * `Map -> (mutable.Map, import scala.collection.mutable)`
     */
    case SpecifiedOwner(sym: Symbol)

  }

  object AutoImport {
    def renamedOrSpecified(sym: Symbol, ownerRename: String)(using
        Context
    ): AutoImport = {
      if (sym.owner.showName == ownerRename) SpecifiedOwner(sym)
      else Renamed(sym, ownerRename)
    }
  }

  def generator(
      pos: SourcePosition,
      text: String,
      tree: Tree,
      namesInScope: NamesInScope,
      config: PresentationCompilerConfig
  )(using ctx: Context): AutoImportsGenerator = {

    val importPos = autoImportPosition(pos, text, tree)
    val renameConfig: Map[SimpleName, String] =
      config.symbolPrefixes.asScala.map { (from, to) =>
        val fullName = from.stripSuffix("/").replace("/", ".")
        val pkg = requiredPackage(fullName)
        (pkg.name.toSimpleName, to.stripSuffix(".").stripSuffix("#"))
      }.toMap

    new AutoImportsGenerator(pos, importPos, namesInScope, renameConfig)
  }

  case class AutoImportEdits(
      nameEdit: Option[l.TextEdit],
      importEdit: Option[l.TextEdit]
  ) {

    def edits: List[l.TextEdit] = List(nameEdit, importEdit).flatten
  }

  object AutoImportEdits {

    def apply(name: l.TextEdit, imp: l.TextEdit): AutoImportEdits =
      AutoImportEdits(Some(name), Some(imp))
    def importOnly(edit: l.TextEdit): AutoImportEdits =
      AutoImportEdits(None, Some(edit))
    def nameOnly(edit: l.TextEdit): AutoImportEdits =
      AutoImportEdits(Some(edit), None)
  }

  class AutoImportsGenerator(
      pos: SourcePosition,
      importPosition: AutoImportPosition,
      namesInScope: NamesInScope,
      renameConfig: Map[SimpleName, String]
  )(using ctx: Context) {

    def forSymbol(symbol: Symbol): Option[List[l.TextEdit]] =
      editsForSymbol(symbol).map(_.edits)

    def editsForSymbol(symbol: Symbol): Option[AutoImportEdits] = {
      inferAutoImport(symbol).map { ai =>
        def mkImportEdit = importEdit(List(ai), importPosition)
        ai match {
          case _: AutoImport.Simple =>
            AutoImportEdits.importOnly(mkImportEdit)
          case AutoImport.SpecifiedOwner(sym)
              if namesInScope.lookupSym(sym.owner).exists =>
            AutoImportEdits.nameOnly(specifyOwnerEdit(sym, sym.owner.showName))
          case AutoImport.SpecifiedOwner(sym) =>
            AutoImportEdits(
              specifyOwnerEdit(sym, sym.owner.showName),
              mkImportEdit
            )
          case AutoImport.Renamed(sym, rename)
              if namesInScope.symbolByName(rename).isDefined =>
            AutoImportEdits.nameOnly(specifyOwnerEdit(sym, rename))
          case AutoImport.Renamed(sym, rename) =>
            AutoImportEdits(specifyOwnerEdit(sym, rename), mkImportEdit)
        }
      }
    }

    private def inferAutoImport(symbol: Symbol): Option[AutoImport] = {
      namesInScope.lookupSym(symbol) match {
        case NamesInScope.Result.Missing => Some(AutoImport.Simple(symbol))
        case NamesInScope.Result.Conflict =>
          val owner = symbol.owner
          val simpleName = owner.name.toSimpleName
          renameConfig.get(simpleName) match {
            case Some(rename) =>
              Some(AutoImport.renamedOrSpecified(symbol, rename))
            case _ => None
          }
        case NamesInScope.Result.InScope => None
      }
    }

    private def specifyOwnerEdit(symbol: Symbol, owner: String): l.TextEdit = {
      val line = pos.startLine
      new l.TextEdit(pos.toLSP, s"$owner.${symbol.nameBackticked}")
    }

    private def importEdit(
        values: List[AutoImport],
        importPosition: AutoImportPosition
    )(using Context): l.TextEdit = {
      val indent = " " * importPosition.indent
      val topPadding =
        if (importPosition.padTop) "\n"
        else ""

      val formatted = values
        .map({
          case AutoImport.Simple(sym) => importName(sym)
          case AutoImport.SpecifiedOwner(sym) => importName(sym.owner)
          case AutoImport.Renamed(sym, rename) =>
            s"${importName(sym.owner.owner)}.{${sym.owner.nameBackticked} => $rename}"
        })
        .map(selector => s"${indent}import $selector")
        .mkString(topPadding, "\n", "\n")

      val editPos = pos.withSpan(Spans.Span(importPosition.offset)).toLSP
      new l.TextEdit(editPos, formatted)
    }

    private def importName(sym: Symbol): String = {
      @tailrec
      def toplevelClashes(sym: Symbol): Boolean = {
        if (sym.owner == NoSymbol || sym.owner.isRoot)
          namesInScope.lookupSym(sym).exists
        else
          toplevelClashes(sym.owner)
      }
      if (toplevelClashes(sym)) s"_root_.${sym.fullNameBackticked}"
      else sym.fullNameBackticked
    }
  }

  private def autoImportPosition(
      pos: SourcePosition,
      text: String,
      tree: Tree
  )(using Context): AutoImportPosition = {

    @tailrec
    def lastPackageDef(
        prev: Option[PackageDef],
        tree: Tree
    ): Option[PackageDef] = {
      tree match {
        case curr @ PackageDef(_, (next: PackageDef) :: Nil)
            if !curr.symbol.isPackageObject =>
          lastPackageDef(Some(curr), next)
        case pkg: PackageDef if !pkg.symbol.isPackageObject => Some(pkg)
        case _ => prev
      }
    }

    def ammoniteObjectBody(tree: Tree)(using Context): Option[Template] = {
      tree match {
        case PackageDef(_, stats) =>
          stats.flatMap {
            case s: PackageDef => ammoniteObjectBody(s)
            case TypeDef(_, t @ Template(defDef, _, _, _))
                if defDef.symbol.showName == "<init>" =>
              Some(t)
            case _ => None
          }.headOption
        case _ => None
      }
    }

    def forScalaSource: Option[AutoImportPosition] = {
      lastPackageDef(None, tree).map { pkg =>
        val lastImportStatement =
          pkg.stats.takeWhile(_.isInstanceOf[Import]).lastOption
        val (lineNumber, padTop) = lastImportStatement match {
          case Some(stm) => (stm.endPos.line + 1, false)
          case None if pkg.pid.symbol.isEmptyPackage =>
            (0, false)
          case None =>
            val pos = pkg.pid.endPos
            val line =
              // pos point at the last NL
              if (pos.endColumn == 0)
                math.max(0, pos.line - 1)
              else
                pos.line + 1
            (line, true)
        }
        val offset = pos.source.lineToOffset(lineNumber)
        new AutoImportPosition(offset, text, padTop)
      }
    }

    def forAmmoniteScript: Option[AutoImportPosition] = {
      ammoniteObjectBody(tree).map { tmpl =>
        val lastImportStatement =
          tmpl.body.takeWhile(_.isInstanceOf[Import]).lastOption
        val (lineNumber, padTop) = lastImportStatement match {
          case Some(stm) => (stm.endPos.line + 1, false)
          case None => (tmpl.self.srcPos.line, false)
        }
        val offset = pos.source.lineToOffset(lineNumber)
        new AutoImportPosition(offset, text, padTop)
      }
    }

    val path = pos.source.path
    val ammonite = if (path.endsWith(".sc.scala")) forAmmoniteScript else None
    ammonite
      .orElse(forScalaSource)
      .getOrElse(AutoImportPosition(0, 0, padTop = false))
  }

}
