/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js tools             **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2016, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */

package org.scalajs.core.tools.javascript

import scala.collection.mutable

import org.scalajs.core.tools.sem._

import org.scalajs.core.tools.linker.LinkingUnit

/** Tracks incrementality breaking changes made during class emitting.
 */
final class IncClassEmitter (semantics: Semantics, outputMode: OutputMode) {
  private var _classEmitter: ScalaJSClassEmitter = _
  private var lastClassesCtorOpt: Set[String] = Set.empty
  private var currentClassesCtorOpt: Set[String] = Set.empty
  private[this] val askedForCtorOpt =
    mutable.Map.empty[String, Set[(String, String, Boolean)]]

  def classEmitter = _classEmitter
  def classEmitter_=(v: ScalaJSClassEmitter): Unit = _classEmitter = v

  def beginRun(unit: LinkingUnit,
      invalidateFunc: (String, String, Boolean) => Unit): ScalaJSClassEmitter = {
    newScalaJSClassEmitter(unit)
    val classesWhoChanged = (lastClassesCtorOpt -- currentClassesCtorOpt) ++
      (currentClassesCtorOpt -- lastClassesCtorOpt)
    val methodsToInvalidate = askedForCtorOpt.retain { (className, callers) =>
      if (classesWhoChanged(className)) {
        // invalidate
        callers.foreach(invalidateFunc.tupled)
        false
      } else {
        true
      }
    }
    classEmitter
  }

  def newScalaJSClassEmitter(unit: LinkingUnit): ScalaJSClassEmitter = {
    classEmitter = new ScalaJSClassEmitter(outputMode, unit, this)
    currentClassesCtorOpt = classEmitter.candidateForJSConstructorOpt
    classEmitter
  }

  def endRun(): Unit = {
    lastClassesCtorOpt = currentClassesCtorOpt
  }

  def usesJSConstructorOpt(className: String): Boolean = {
    currentClassesCtorOpt(className)
  }

  def usesJSConstructorOpt(className: String, callerClassName: String,
      methodName: String, isStatic: Boolean): Boolean = {
    val currentEntry = askedForCtorOpt.getOrElse(className, Set.empty)
    val newEntry = currentEntry + ((callerClassName, methodName, isStatic))
    askedForCtorOpt += (className -> newEntry)

    usesJSConstructorOpt(className)
  }
}
