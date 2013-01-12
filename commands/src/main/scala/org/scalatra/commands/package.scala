package org.scalatra

import validation.ValidationError
import scalaz._
import Scalaz._
import org.json4s.JsonAST.{JValue, JNothing}
import java.util.Date
import org.joda.time.{ DateTime, DateTimeZone }

package object commands extends DefaultValues {

  type FieldValidation[T] = ValidationNEL[ValidationError, T]
  type StringValidation[T] = ValidationNEL[String, T]

  type Validator[T] = T => StringValidation[T]
  type FieldValidator[T] = T => FieldValidation[T]

  type BindingAction = () => Any

  implicit val minDateDefault: org.scalatra.DefaultValue[Date] = default(new Date(0))
  implicit val minDateTimeDefault: org.scalatra.DefaultValue[DateTime] = default(new DateTime(0).withZone(DateTimeZone.UTC))
  implicit val jsonDefault: org.scalatra.DefaultValue[JValue] = default(JNothing)
  
  implicit def validatorMonoid[A]: Monoid[Validator[A]] = new Monoid[Validator[A]] {
    def zero = _.success
    def append(x: Validator[A], y: => Validator[A]) : Validator[A] = { a: A => 
      (x(a) |@| y(a))((_, a) => a)
    }
  }
}

