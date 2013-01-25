package org.scalatra
package validation

import org.apache.commons.validator.routines.EmailValidator
import scala.util.matching.Regex
import java.util.Locale._
import scala.util.control.Exception._
import java.net.URI
import org.apache.commons.validator.routines.UrlValidator
import scalaz._
import Scalaz._
import mojolly.inflector.InflectorImports._
import commands.{FieldValidation,Validator}

object Validators {
  def formatFailure[A](fieldName: String, messageFormat: String) =
      ValidationError(messageFormat.format(fieldName.humanize), FieldName(fieldName), ValidationFail)

  /**
  * Must be a non-empty [String]. null, " ", and "" are not allowed.
  */
  def nonEmptyString(message: => String = "%s must be present.")(value: String) =
    if(value != null && value.trim.nonEmpty) {println("nonEmpty: " + value) ; value.successNel} else message.failNel

  /**
   * Must be non-null.
   */
  def notNull(message: => String)(value: AnyRef) = 
    if(value != null) value.successNel else message.failNel
  
  /**
   * Must be a collection which isn't empty.
   */
  def nonEmptyCollection[TResult <: Traversable[_]](message: => String = "%s must not be empty.")(value: TResult) = 
    if(value.nonEmpty) value.successNel else message.failNel

  /**
   * Must be a valid email as determined by org.apache.commons.validator.routines.EmailValidator
   */
  def validEmail(message: => String = "%s must be a valid email.")(value: String) =
    if(EmailValidator.getInstance.isValid(value)) value.successNel else message.failNel

  /**
   * Must be a valid absolute URL, parseable by the Apache Commons URI class.
   */
  def validAbsoluteUrl(allowLocalHost: Boolean, schemes: String*) =
    buildUrlValidator(true, allowLocalHost, schemes: _*)

  /**
   * Must be a valid URL, parseable by the Apache Commons URI class.
   */
  def validUrl(allowLocalHost: Boolean, schemes: String*) =
    buildUrlValidator(false, allowLocalHost, schemes: _*)

  /**
   * Must match the regex.
   */
  def validFormat(regex: Regex, message: => String = "%s is invalid.")(value: String) = 
    if(regex.findFirstIn(value).isDefined) value.successNel else message.failNel

  /**
   * The confirmation fieldName must have a true value.
   */
  def validConfirmation(confirmationFieldName: String, confirmationValue: => String)(value: String) = 
    if(value == confirmationValue) 
      value.successNel 
    else 
      ("%s must match " + confirmationFieldName.underscore.humanize.toLowerCase(ENGLISH) + ".").failNel

  /**
   * Must be greater than the min param.
   */
  def greaterThan[T <% Ordered[T]](min: T)(value: T) = 
    if(value > min) value.successNel else ("%s must be greater than " + min.toString).failNel

  /**
   * Must be less than the max param.
   */
  def lessThan[T <% Ordered[T]](max: T)(value: T) = 
    if(value < max) value.successNel else ("%s must be less than " + max.toString).failNel

  /**
   * Must be greater than or equal to the min param.
   */
  def greaterThanOrEqual[T <% Ordered[T]](min: T)(value: T) = 
    if(value >= min) value.successNel else ("%s must be greater than or equal to " + min.toString).failNel

  /**
   * Must be less than or equal to the max param.
   */
  def lessThanOrEqual[T <% Ordered[T]](max: T)(value: T) = 
    if(value <= max) value.successNel else ("%s must be less than or equal to " + max.toString).failNel

  /**
   * Must have a minimum length of min.
   */
  def minLength(min: Int)(value: String) = 
    if(value.size >= min) 
      value.successNel 
    else 
      ("%s must be at least " + min.toString + " characters long.").failNel

  /**
   * Must be included in the expected collection.
   */
  def oneOf[TResult](expected: TResult*) : Validator[TResult] = (value: TResult) => {
      if(expected contains value)
        value.success
      else
        ("%s must be one of " + expected.mkString("[", ", ", "]")).fail
  }

  /**
   * Checks if the value of the data is a value of the specified enum.
   */ 
  def enumValue(enum: Enumeration) = oneOf(enum.values.map(_.toString).toSeq: _*)

  private def buildUrlValidator(absolute: Boolean, allowLocalHost: Boolean, schemes: String*) : Validator[String] = (url: String) => {
    if((allCatch opt {
        val u = URI.create(url).normalize()
        !absolute || u.isAbsolute
       }).isDefined && (allowLocalHost || UrlValidator.getInstance().isValid(url)))
      url.success
    else
    "%s must be a valid url.".fail
  }
}
