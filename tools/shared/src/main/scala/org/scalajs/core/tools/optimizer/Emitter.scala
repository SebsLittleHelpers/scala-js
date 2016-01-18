/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js tools             **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2014, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package org.scalajs.core.tools.optimizer

import scala.annotation.tailrec

import scala.collection.mutable

import org.scalajs.core.tools.sem._
import org.scalajs.core.tools.logging._
import org.scalajs.core.tools.sourcemap.JSFileBuilder

import org.scalajs.core.tools.corelib.CoreJSLibs
import org.scalajs.core.tools.javascript
import javascript.{Trees => js, OutputMode}

import org.scalajs.core.ir.{ClassKind, Definitions, Position}
import org.scalajs.core.ir.Definitions._
import org.scalajs.core.ir.{Trees => ir}

/** Emits a desugared JS tree to a builder */
final class Emitter(semantics: Semantics, outputMode: OutputMode) {
  import Emitter._

  @deprecated("Use the overload with an explicit OutputMode", "0.6.2")
  def this(semantics: Semantics) =
    this(semantics, OutputMode.ECMAScript51Global)

  private var classEmitter: javascript.ScalaJSClassEmitter = _
  private val incClassEmitter: javascript.IncClassEmitter =
    new javascript.IncClassEmitter(semantics, outputMode)

  private val classCaches = mutable.Map.empty[List[String], ClassCache]

  private[this] var statsClassesReused: Int = 0
  private[this] var statsClassesInvalidated: Int = 0
  private[this] var statsMethodsReused: Int = 0
  private[this] var statsMethodsInvalidated: Int = 0

  def emitAll(unit: LinkingUnit, builder: JSFileBuilder,
      logger: Logger): Unit = {
    emitPrelude(builder, logger)
    emit(unit, builder, logger)
    emitPostlude(builder, logger)
  }

  def emitCustomHeader(customHeader: String, builder: JSFileBuilder): Unit =
    emitLines(customHeader, builder)

  def emitPrelude(builder: JSFileBuilder, logger: Logger): Unit = {
    outputMode match {
      case OutputMode.ECMAScript51Global =>
      case OutputMode.ECMAScript51Isolated | OutputMode.ECMAScript6 =>
        builder.addLine("(function(){")
      case OutputMode.ECMAScript6StrongMode =>
        builder.addLine("(function(__this, __ScalaJSEnv, __global, " +
            "$jsSelect, $jsAssign, $jsDelete, $propertiesOf, $weakFun) {")
    }

    builder.addLine("'use strict';")
    outputMode match {
      case OutputMode.ECMAScript6StrongMode =>
        builder.addLine("'use strong';")
      case _ =>
        builder.addFile(CoreJSLibs.lib(semantics, outputMode))
    }
  }

  def emit(unit: LinkingUnit, builder: JSTreeBuilder, logger: Logger): Unit = {
    classEmitter = incClassEmitter.newScalaJSClassEmitter(unit)
    incClassEmitter.beginRun(invalidateMethodCache(classEmitter))
    startRun()
    try {
      val orderedClasses = unit.classDefs.sortWith(compareClasses)
      outputMode match {
        case OutputMode.ECMAScript6StrongMode =>
          builder match {
            case builder: JSFileBuilder =>
              emitStrongMode(orderedClasses, builder)
            case _ =>
              throw new IllegalArgumentException(
                  "Emitting to Strong Mode requires a JSFileBuilder")
          }
        case _ =>
          for (classInfo <- orderedClasses)
            emitLinkedClass(classInfo, builder)
      }
    } finally {
      endRun(logger)
      incClassEmitter.endRun
      classEmitter = null
    }
  }

  def emitPostlude(builder: JSFileBuilder, logger: Logger): Unit = {
    outputMode match {
      case OutputMode.ECMAScript51Global =>
      case OutputMode.ECMAScript51Isolated | OutputMode.ECMAScript6 =>
        builder.addLine("}).call(this);")
      case OutputMode.ECMAScript6StrongMode =>
        builder.addLine("})(this,")
        builder.addLine("  (typeof __ScalaJSEnv !== 'undefined') ? __ScalaJSEnv : void 0,")
        builder.addLine("  (typeof global !== 'undefined') ? global : void 0,")
        builder.addLine("  function(x, p) { 'use strict'; return x[p]; },")
        builder.addLine("  function(x, p, v) { 'use strict'; x[p] = v; },")
        builder.addLine("  function(x, p) { 'use strict'; delete x[p]; },")
        builder.addLine("  function(x) { 'use strict'; const r = []; for (const p in x) r['push'](p); return r; },")
        builder.addLine("  function(f) { 'use strict'; return function(...args) { return f['apply'](void 0, args); } });")
    }
  }

  def emitCustomFooter(customFooter: String, builder: JSFileBuilder): Unit =
    emitLines(customFooter, builder)

  private def compareClasses(lhs: LinkedClass, rhs: LinkedClass) = {
    val lhsAC = lhs.ancestors.size
    val rhsAC = rhs.ancestors.size
    if (lhsAC != rhsAC) lhsAC < rhsAC
    else lhs.encodedName.compareTo(rhs.encodedName) < 0
  }

  private def startRun(): Unit = {
    statsClassesReused = 0
    statsClassesInvalidated = 0
    statsMethodsReused = 0
    statsMethodsInvalidated = 0
    classCaches.valuesIterator.foreach(_.startRun())
  }

  private def endRun(logger: Logger): Unit = {
    logger.debug(
        s"Emitter: Class tree cache stats: reused: $statsClassesReused -- "+
        s"invalidated: $statsClassesInvalidated")
    logger.debug(
        s"Emitter: Method tree cache stats: resued: $statsMethodsReused -- "+
        s"invalidated: $statsMethodsInvalidated")
    classCaches.retain((_, c) => c.cleanAfterRun())
  }

  private def emitLinkedClass(
      linkedClass: LinkedClass, builder: JSTreeBuilder): Unit = {

    def addTree(tree: js.Tree): Unit = builder.addJSTree(tree)

    val className = linkedClass.encodedName
    val classCache = getClassCache(linkedClass.ancestors)
    val classTreeCache = classCache.getCache(linkedClass.version)
    val kind = linkedClass.kind

    // Statics
    for (m <- linkedClass.staticMethods) {
      val methodCache = classCache.getStaticCache(m.info.encodedName)

      addTree(methodCache.getOrElseUpdate(m.version,
        classEmitter.genMethod(className, m.tree)))
    }

    if (linkedClass.hasInstances && kind.isAnyScalaJSDefinedClass) {
      val ctor = classTreeCache.constructor.getOrElseUpdate(
          classEmitter.genConstructor(linkedClass))
      // Normal methods
      val allMethods = linkedClass.memberMethods
      val methodsDefs = {
        if (incClassEmitter.usesJSConstructorOpt(className))
          allMethods.filterNot(x => isConstructorName(x.info.encodedName))
        else
          allMethods
      }

      val memberMethods = for (m <- methodsDefs) yield {
        val methodCache = classCache.getMethodCache(m.info.encodedName)

        methodCache.getOrElseUpdate(m.version,
            classEmitter.genMethod(className, m.tree))
      }

      // Exported Members
      val exportedMembers = classTreeCache.exportedMembers.getOrElseUpdate(
          classEmitter.genExportedMembers(linkedClass))

      outputMode match {
        case OutputMode.ECMAScript51Global | OutputMode.ECMAScript51Isolated =>
          addTree(ctor)
          memberMethods.foreach(addTree)
          addTree(exportedMembers)

        case OutputMode.ECMAScript6 | OutputMode.ECMAScript6StrongMode =>
          val allMembersBlock = js.Block(
              ctor :: memberMethods ::: exportedMembers :: Nil)(Position.NoPosition)
          val allMembers = allMembersBlock match {
            case js.Block(members) => members
            case js.Skip()         => Nil
            case oneMember         => List(oneMember)
          }
          addTree(classEmitter.genES6Class(linkedClass, allMembers))
      }
    }

    if (classEmitter.needInstanceTests(linkedClass)) {
      addTree(classTreeCache.instanceTests.getOrElseUpdate(js.Block(
          classEmitter.genInstanceTests(linkedClass),
          classEmitter.genArrayInstanceTests(linkedClass)
      )(linkedClass.pos)))
    }

    if (linkedClass.hasRuntimeTypeInfo) {
      addTree(classTreeCache.typeData.getOrElseUpdate(
          classEmitter.genTypeData(linkedClass)))
    }

    if (linkedClass.hasInstances && kind.isClass && linkedClass.hasRuntimeTypeInfo)
      addTree(classTreeCache.setTypeData.getOrElseUpdate(
          classEmitter.genSetTypeData(linkedClass)))

    if (linkedClass.kind.hasModuleAccessor)
      addTree(classTreeCache.moduleAccessor.getOrElseUpdate(
          classEmitter.genModuleAccessor(linkedClass)))

    addTree(classTreeCache.classExports.getOrElseUpdate(
        classEmitter.genClassExports(linkedClass)))
  }

  private def emitStrongMode(orderedClasses: List[LinkedClass],
      builder: JSFileBuilder): Unit = {

    assert(outputMode == OutputMode.ECMAScript6StrongMode)

    def addTree(tree: js.Tree): Unit = builder.addJSTree(tree)

    var remainingFromLib =
      CoreJSLibs.lib(semantics, outputMode).content.split('\n').toList

    @tailrec
    def emitFromLibUntil(marker: String): Unit = {
      remainingFromLib match {
        case `marker` :: rest =>
          remainingFromLib = rest
        case head :: rest =>
          builder.addLine(head)
          remainingFromLib = rest
          emitFromLibUntil(marker)
        case Nil =>
      }
    }

    emitFromLibUntil("///INSERT DECLARE TYPE DATA HERE///")
    for (linkedClass <- orderedClasses) {
      builder.addJSTree(classEmitter.genDeclareTypeData(linkedClass))
    }

    emitFromLibUntil("///INSERT DECLARE MODULES HERE///")
    for (linkedClass <- orderedClasses) {
      if (linkedClass.kind.hasModuleAccessor)
        builder.addJSTree(classEmitter.genDeclareModule(linkedClass))
    }

    emitFromLibUntil("///INSERT IS AND AS FUNCTIONS HERE///")
    for (linkedClass <- orderedClasses) {
      if (classEmitter.needInstanceTests(linkedClass)) {
        val classTreeCache = getClassTreeCache(linkedClass)
        addTree(classTreeCache.instanceTests.getOrElseUpdate(js.Block(
            classEmitter.genInstanceTests(linkedClass),
            classEmitter.genArrayInstanceTests(linkedClass)
        )(linkedClass.pos)))
      }
    }

    emitFromLibUntil("///INSERT CLASSES HERE///")
    for (linkedClass <- orderedClasses) {
      val className = linkedClass.encodedName
      val classCache = getClassCache(linkedClass.ancestors)
      val classTreeCache = classCache.getCache(linkedClass.version)
      val kind = linkedClass.kind

      // Statics
      val staticMethods = for (m <- linkedClass.staticMethods) yield {
        val methodCache = classCache.getStaticCache(m.info.encodedName)
        methodCache.getOrElseUpdate(m.version,
            classEmitter.genMethod(className, m.tree))
      }

      if (linkedClass.hasInstances && kind.isAnyScalaJSDefinedClass) {
        val ctor = classTreeCache.constructor.getOrElseUpdate(
            classEmitter.genConstructor(linkedClass))

        // Normal methods
        val memberMethods = for (m <- linkedClass.memberMethods) yield {
          val methodCache = classCache.getMethodCache(m.info.encodedName)
          methodCache.getOrElseUpdate(m.version,
              classEmitter.genMethod(className, m.tree))
        }

        // Exported Members
        val exportedMembers = classTreeCache.exportedMembers.getOrElseUpdate(
            classEmitter.genExportedMembers(linkedClass))

        // Module accessor
        val moduleAccessor = {
          if (linkedClass.kind.hasModuleAccessor) {
            classTreeCache.moduleAccessor.getOrElseUpdate(
                classEmitter.genModuleAccessor(linkedClass))
          } else {
            js.Skip()(linkedClass.pos)
          }
        }

        val allMembersBlock = js.Block(
            ctor :: memberMethods ::: exportedMembers ::
            moduleAccessor :: staticMethods)(Position.NoPosition)
        val allMembers = allMembersBlock match {
          case js.Block(members) => members
          case js.Skip()         => Nil
          case oneMember         => List(oneMember)
        }
        addTree(classEmitter.genES6Class(linkedClass, allMembers))
      } else if (staticMethods.nonEmpty) {
        addTree(classEmitter.genStaticsES6Class(linkedClass, staticMethods))
      }
    }

    emitFromLibUntil("///INSERT CREATE TYPE DATA HERE///")
    for (linkedClass <- orderedClasses) {
      if (linkedClass.hasRuntimeTypeInfo) {
        val classTreeCache = getClassTreeCache(linkedClass)
        addTree(classTreeCache.typeData.getOrElseUpdate(
            classEmitter.genTypeData(linkedClass)))
      }
    }

    emitFromLibUntil("///INSERT EXPORTS HERE///")
    for (linkedClass <- orderedClasses) {
      val classTreeCache = getClassTreeCache(linkedClass)
      addTree(classTreeCache.classExports.getOrElseUpdate(
          classEmitter.genClassExports(linkedClass)))
    }

    emitFromLibUntil("///THE END///")
  }

  // Helpers

  private def invalidateMethodCache(clssEmitter: javascript.ScalaJSClassEmitter)(
      enclosingClassName: String, methodName: String,
      isStatic: Boolean): Unit = {
    val linkedClass = clssEmitter.linkedClassByName(enclosingClassName)
    val classCache = getClassCache(linkedClass.ancestors)
    val classTreeCache = classCache.getCache(linkedClass.version)
    val ctorExportDef = javascript.ScalaJSClassEmitter.ConstructorExportDefName
    val exportedMember = javascript.ScalaJSClassEmitter.ExportedMemberName
    (methodName, isStatic) match {
        case (`ctorExportDef` | `exportedMember`, _) =>
          classTreeCache.exportedMembers.invalidate
        case (_, true) =>
          classCache.getStaticCache(methodName).invalidate
        case _ =>
          classCache.getMethodCache(methodName).invalidate

    }
  }

  private def getClassTreeCache(linkedClass: LinkedClass): DesugaredClassCache =
    getClassCache(linkedClass.ancestors).getCache(linkedClass.version)

  private def getClassCache(ancestors: List[String]) =
    classCaches.getOrElseUpdate(ancestors, new ClassCache)

  private def emitLines(str: String, builder: JSFileBuilder): Unit = {
    @tailrec def emitNextLine(index: Int): Unit = {
      val endOfLine = str.indexOf('\n', index)
      if (endOfLine != -1) {
        builder.addLine(str.substring(index, endOfLine))
        emitNextLine(endOfLine + 1)
      } else {
        builder.addLine(str.substring(index, str.length))
      }
    }
    if (str != "")
      emitNextLine(0)
  }

  // Caching

  private final class ClassCache {
    private[this] var _cache: DesugaredClassCache = null
    private[this] var _lastVersion: Option[String] = None
    private[this] var _cacheUsed = false

    private[this] val _staticCaches = mutable.Map.empty[String, MethodCache]
    private[this] val _methodCaches = mutable.Map.empty[String, MethodCache]

    def startRun(): Unit = {
      _cacheUsed = false
      _staticCaches.valuesIterator.foreach(_.startRun())
      _methodCaches.valuesIterator.foreach(_.startRun())
    }

    def getCache(version: Option[String]): DesugaredClassCache = {
      if (_cache == null || _lastVersion.isEmpty || _lastVersion != version) {
        statsClassesInvalidated += 1
        _lastVersion = version
        _cache = new DesugaredClassCache
      } else {
        statsClassesReused += 1
      }
      _cacheUsed = true
      _cache
    }

    def getStaticCache(encodedName: String): MethodCache =
      _staticCaches.getOrElseUpdate(encodedName, new MethodCache)

    def getMethodCache(encodedName: String): MethodCache =
      _methodCaches.getOrElseUpdate(encodedName, new MethodCache)

    def cleanAfterRun(): Boolean = {
      _staticCaches.retain((_, c) => c.cleanAfterRun())
      _methodCaches.retain((_, c) => c.cleanAfterRun())

      if (!_cacheUsed)
        _cache = null

      _staticCaches.nonEmpty || _methodCaches.nonEmpty || _cacheUsed
    }
  }

  private final class MethodCache {
    private[this] var _tree: js.Tree = null
    private[this] var _lastVersion: Option[String] = None
    private[this] var _cacheUsed = false

    def startRun(): Unit = _cacheUsed = false

    def getOrElseUpdate(version: Option[String], v: => js.Tree): js.Tree = {
      if (_tree == null || _lastVersion.isEmpty || _lastVersion != version) {
        statsMethodsInvalidated += 1
        _tree = v
        _lastVersion = version
      } else {
        statsMethodsReused += 1
      }
      _cacheUsed = true
      _tree
    }

    def invalidate: Unit = _tree = null

    def cleanAfterRun(): Boolean = _cacheUsed
  }
}

object Emitter {
  private final class DesugaredClassCache {
    val constructor = new OneTimeCache[js.Tree]
    val exportedMembers = new OneTimeCache[js.Tree]
    val instanceTests = new OneTimeCache[js.Tree]
    val typeData = new OneTimeCache[js.Tree]
    val setTypeData = new OneTimeCache[js.Tree]
    val moduleAccessor = new OneTimeCache[js.Tree]
    val classExports = new OneTimeCache[js.Tree]
  }

  private final class OneTimeCache[A >: Null] {
    private[this] var value: A = null
    def getOrElseUpdate(v: => A): A = {
      if (value == null)
        value = v
      value
    }

    def invalidate: Unit = value = null
  }
}
