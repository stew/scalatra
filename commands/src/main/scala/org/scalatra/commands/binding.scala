package org.scalatra
package commands

import org.scalatra.util.conversion._
import validation._
import java.util.Date
import scalaz._
import Scalaz._
import org.joda.time.DateTime
import java.util.concurrent.atomic.AtomicReference
import scala.util.matching.Regex

class BindingException(message: String) extends ScalatraException(message)

trait BindingSyntax {

  implicit def asType[T](name: String) = QueryBinder[T](name)

  def asBoolean(name: String) = QueryBinder[Boolean](name)
  def asByte(name: String) = QueryBinder[Byte](name)
  def asShort(name: String) = QueryBinder[Short](name)
  def asInt(name: String) = QueryBinder[Int](name)
  def asLong(name: String) = QueryBinder[Long](name)
  def asFloat(name: String) = QueryBinder[Float](name)
  def asDouble(name: String) = QueryBinder[Double](name)
  def asBigDecimal(name: String) = QueryBinder[BigDecimal](name)
  def asString(name: String) = QueryBinder[String](name)
  def asDate(name: String) = QueryBinder[Date](name)
  def asDateTime(name: String) = QueryBinder[DateTime](name)
  def asSeq[T](name: String) = QueryBinder[Seq[T]](name)
}

object BindingSyntax extends BindingSyntax

/**
* Commonly-used field implementations factory.
*
* @author mmazzarolo
*/
trait BindingImplicits extends DefaultImplicitConversions {

  implicit def stringToDateTime(implicit df: DateParser = JodaDateFormats.Web): TypeConverter[String, DateTime] =
    safeOption(df.parse)

  implicit def stringToDate(implicit df: DateParser = JodaDateFormats.Web): TypeConverter[String, Date] =
    safeOption(df.parse(_).map(_.toDate))

  implicit def stringToSeqDateTime(implicit df: DateParser = JodaDateFormats.Web): TypeConverter[String, Seq[DateTime]] =
    stringToSeq(stringToDateTime)

  implicit def stringToSeqDate(implicit df: DateParser = JodaDateFormats.Web): TypeConverter[String, Seq[Date]] =
    stringToSeq(stringToDate)

}

object BindingImplicits extends BindingImplicits
