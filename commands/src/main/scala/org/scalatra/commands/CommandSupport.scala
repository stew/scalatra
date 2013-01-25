package org.scalatra
package commands

import util.{ParamsValueReaderProperties, MultiMap}
import collection.mutable
import java.util.Date
import org.joda.time.DateTime
import collection.JavaConverters._
import java.util.concurrent.ConcurrentHashMap
import grizzled.slf4j.Logger
import org.json4s._

/**
* Support for [[org.scalatra.commands.Command]] binding and validation.
*/
trait CommandSupport extends ParamsValueReaderProperties { this: ScalatraSyntax =>

  private[this] val commandFactories: mutable.ConcurrentMap[Class[_], () => Command] = new ConcurrentHashMap[Class[_], () => Command].asScala

  def registerCommand[T <: Command](cmd: => T)(implicit mf: Manifest[T]) {
    commandFactories += (mf.erasure -> (() => cmd))
  }
/*
  /**
   * Create and bind a [[org.scalatra.commands.Command]] of the given type with the current Scalatra params.
   *
   * For every command type, creation and binding is performed only once and then stored into
   * a request attribute.
   */

 // STU: problematic because we don't yet have the type we need so we don't know how to search for the implicits
  def command[T <: Command](implicit mf: Manifest[T], jbinding: Binding[T#R, JValue], pbinding: Binding[A, Params]): T = {
    def createCommand = commandFactories.get(mf.erasure).map(_()).getOrElse(mf.erasure.newInstance()).asInstanceOf[T]
    commandOption[A, T] getOrElse bindCommand[A,T](createCommand)
  }
  /**
   * Create and bind a [[org.scalatra.commands.Command]] of the given type with the current Scalatra params.
   *
   * For every command type, creation and binding is performed only once and then stored into
   * a request attribute.
   */
 */
  protected def bindCommand[T <: Command](newCommand: T)(/*implicit mf: Manifest[T], */implicit pbinding: Binding[TypedBinder[newCommand.fields.R], Params]): T = {
    newCommand.bindTo(BodySource(params), multiParams, request.headers)
//    requestProxy.update(commandRequestKey[T], newCommand)
    newCommand
  }

  def commandOption[T <: Command : Manifest] : Option[T] = requestProxy.get(commandRequestKey[T]).map(_.asInstanceOf[T])


  private[commands] def requestProxy: mutable.Map[String, Any] = request

  private[commands] def commandRequestKey[T <: Command : Manifest] = "_command_" + manifest[T].erasure.getName
/*  stew: TODO
  private class CommandRouteMatcher[A, T <: Command ](implicit mf: Manifest
[T], jbinding: Binding[A, JValue], pbinding: Binding[A, Params]) extends RouteMatcher {

    override def apply(requestPath: String) = command[T].fields.validate.fold(f => None, s => Some(MultiMap()))
  }
  /**
   * Create a [[org.scalatra.RouteMatcher]] that evaluates '''true''' only if a command is valid.
   */
  def ifValid[A, T <: Command](implicit mf: Manifest[T], jbinding: Binding[A, JValue], pbinding: Binding[A, Params]): RouteMatcher = new CommandRouteMatcher[A, T]
 */

}

trait ParamsOnlyCommandSupport[A] extends CommandSupport { this: ScalatraSyntax =>
  type CommandType = ParamsOnlyCommand
}

