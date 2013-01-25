package org.scalatra
package commands

import util._
import conversion._
import collection.immutable
import org.joda.time.DateTime
import java.util.Date
import scalaz._
import Scalaz._
import validation._
import org.json4s._

case class BodySource[T](data: T) {
  def read[A](field: String)(implicit r: T => ValueReader[T, A]) : Either[String, Option[A]] = r(data).read(field)
}


/**
* Trait that identifies a ''Command object'', i.e. a Scala class instance which fields are bound to external parameters
* taken from Scalatra' __params__ object.
*
* An usage example can be seen below:
* {{{
* class PersonForm extends Command {
*
*  import Command._
*
*  val name = bind[String]("f_name")
*  val surname = bind[String]("f_surname")
*  val age = bind[Int]("f_age")
*  val registeredOn = bind[Date]("f_reg_date" -> "yyyyMMdd")
* }
* }}}
*
* In the example above, class field ''name'' will be bound, at runtime, with a parameter named ''f_name'' and so on.
* The binding is typed and for every registered type `T` (see [[org.scalatra.util.conversion.DefaultImplicitConversions]] for
* a list of all availables) an automatic conversion `(String) => T` will take place during binding phase.
*
* After that binding has been performed (i.e. after that [[org.scalatra.commands.Command#bindTo()]] has been called)
* on a specific instance, it is possible retrieve field values as [[scalaz.Validation]], i.e.:
*
* {{{
* val form = new PersonForm
* form.doBinding(params)
* val registrationDate = form.registeredOn.value.getOrElse(new Date())
* }}}
*
*
* @author mmazzarolo
* @version 0.1
*
*/
trait Command extends BindingSyntax with ParamsValueReaderProperties {
  private[this] var preBindingActions: Seq[BindingAction] = Nil

  private[this] var postBindingActions: Seq[BindingAction] = Nil

  val fields: Binder

  private[this] var _errors: Seq[ValidationError] = Nil

  var commandName = getClass.getSimpleName

  var commandDescription = ""

  def bindTo[S](data: BodySource[S], params: MultiParams, headers: Map[String,String])
            (implicit binding: Binding[TypedBinder[fields.R],S]) = {
    binding(fields.asInstanceOf[TypedBinder[fields.R]], data, params, headers)
  }

  /**
   * Check whether this command is valid.
   */
  def isValid = errors.isEmpty
  def isInvalid = errors.nonEmpty

  /**
   * Return a Map of all field command error keyed by field binding name (NOT the name of the variable in command
   * object).
   */
  def errors: Seq[ValidationError] = _errors

  /**
   * Perform command as afterBinding task.
   */
  afterBinding {
    _errors = fields.errors
    if(isValid) {
      
    }
  }

  /**
   * Add an action that will be evaluated before field binding occurs.
   */
  protected def beforeBinding(action: => Any) {
    preBindingActions = preBindingActions :+ (() => action)
  }

  /**
   * Add an action that will be evaluated after field binding has been done.
   */
  protected def afterBinding(action: => Any) {
    postBindingActions = postBindingActions :+ (() => action)
  }


  private def doBeforeBindingActions() = preBindingActions.foreach(_.apply())

  private def doAfterBindingActions() = postBindingActions.foreach(_.apply())

// STEW: TODO  override def toString: String = "%s(bindings: [%s])".format(getClass.getName, bindings.mkString(", "))
}

trait ParamsOnlyCommand extends TypeConverterFactories with Command {
  type CommandTypeConverterFactory[T] = TypeConverterFactory[T]
}

//trait ForceFromParams { self: Command =>
//
//  def forceFromParams: Set[String]
//}
//
//trait ForceFromHeaders { self: Command =>
//
//  def forceFromHeaders: Set[String]
//}


