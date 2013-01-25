package org.scalatra
package commands

import util._
import conversion._
import validation._
import scalaz._
import Scalaz._
import mojolly.inflector.InflectorImports._
import org.scalatra.util.RicherString._

object DefVal {
  def apply[T:Manifest](prov: => T) = new DefVal(prov)
}

class DefVal[T:Manifest](valueProvider: => T) {
  lazy val value = valueProvider
}

class BinderOps[V](v: V) {
  def bindTo[S](data: BodySource[S], params: MultiParams, headers: Map[String,String])(implicit binding: Binding[V,S]) : Unit = {
    binding.apply(v, data, params, headers)
  }
}

trait Binding[V, S] {
  def apply(binder: V, data: BodySource[S], params: MultiParams, headers: Map[String,String]) : Unit
}

object Binding {
  implicit def chainBinding[A, B, AA <: TypedBinder[A], BB <: TypedBinder[B], S](implicit abinding: Binding[AA, S], bbinding: Binding[BB,S]) : Binding[ChainedBinder[A,  AA, B, BB], S] = new Binding[ChainedBinder[A, AA, B, BB], S] {
    def apply(x: ChainedBinder[A, AA, B, BB], data: BodySource[S], params: MultiParams, headers: Map[String, String]) = {
      abinding.apply(x.v._1, data, params, headers)
      bbinding.apply(x.v._2, data, params, headers)
    }
  }

  implicit def mapBinding[A, F <: Binder, S](implicit bbinding: Binding[F, S]) : Binding[MapBinder[A, F], S] = new Binding[MapBinder[A, F], S] {
    def apply(binder: MapBinder[A,F], data: BodySource[S], params: MultiParams, headers: Map[String,String]) = {
      bbinding.apply(binder.previous, data, params, headers)
    }
  }

  implicit def headerBinding[A, S](implicit tcf: TypeConverterFactory[A]): Binding[HeaderBinder[A],S] = new Binding[HeaderBinder[A],S] {
    def apply(binder: HeaderBinder[A], data: BodySource[S], params: MultiParams, headers: Map[String,String]) = {
      val tc: TypeConverter[String, A] = tcf.resolveStringParams
      val result = for {
        header <- headers.get(binder.field).toSuccess(ValidationError("%s is required".format(binder.field), binder.field.some, NotFound.some))
        converted <- tc(header).toSuccess(ValidationError("error converting %s".format(binder.field), binder.field.some, ValidationFail.some))
      } yield(converted)
      binder.bound = result
    }
  }

  implicit def bodyBinding[A, S](binder: BodyBinder[A])(implicit r: S => ValueReader[S, A]): Binding[BodyBinder[A],S] = new Binding[BodyBinder[A],S] {
    def apply(binder: BodyBinder[A], data: BodySource[S], params: MultiParams, headers: Map[String,String]) = {
      val result = data.read(binder.field) match {
        case Left(e) => ValidationError("unknown error fetching %s".format(binder.field), binder.field.some, UnknownError.some).fail
        case Right(None) => ValidationError("%s is required".format(binder.field), binder.field.some, NotFound.some).fail
        case Right(Some(a)) => a.success
      }
      binder.bound = result
    }
  }

  implicit def queryBinding[A, S](implicit tcf: TypeConverterFactory[A], multiParams: MultiParams => ValueReader[MultiParams, Seq[String]]): Binding[QueryBinder[A],S] = new Binding[QueryBinder[A],S] {
    def apply(binder: QueryBinder[A], data: BodySource[S], params: MultiParams, headers: Map[String,String]) = {
      val result = params.read(binder.field) match {
        case Left(e) => ValidationError("unknown error fetching %s".format(binder.field), binder.field.some, UnknownError.some).fail
        case Right(None) => ValidationError("%s is required".format(binder.field), binder.field.some, NotFound.some).fail
        case Right(Some(a)) => tcf.resolveMultiParams(a).toSuccess(ValidationError("unknown error fetching %s".format(binder.field), binder.field.some, UnknownError.some))
      }
      binder.bound = result
    }
  }
}

object Binder {
  implicit def toOps[V <: Binder](b: V) : BinderOps[V] = new BinderOps(b)
}

case class ~[A, B](_1: A, _2: B) 


sealed trait Binder {
  type R
  var result : FieldValidation[R] = _


  /**
    Used to document which field caused a failure
    */
  def field : String

  /**
    Validate the value.
    this separateion allows us to have a two staged bind + validate.
    */
  def validate: FieldValidation[R]

  def errors: IndexedSeq[ValidationError] = validate.fold (IndexedSeq(_), x => IndexedSeq())
}

sealed trait TypedBinder[A] extends Binder {
  type R=A

  def map[B](f: R => B) = ValueMap[B, TypedBinder[A]](this, f)
  def flatMap[B](f: R => TypedBinder[B]) = ValueFlatMap[B, TypedBinder[A]](this, f)

  def ~[B](next: TypedBinder[B]) = ChainedBinder[A, TypedBinder[A], B, TypedBinder[B]](new ~(this, next))

  /**
    validate a new validation to this binder
    */
  def validateWith(validators: Validator[R]*) : TypedBinder[A] = {
    // This could just be validators.suml if we accept a List[Validator[A]] instead of varargs, hmm
    // but anyway... collapses all the validators into a single validator which returns multiple possible failures.
    // which perhaps we don't want, we might perhaps just want the first failure
    val validator : Validator[A] = if(validators.isEmpty) Monoid[Validator[A]].zero else validators.tail.foldLeft(validators.head)(_ |+| _)

    // we ask for validators which fail with a string.  we compose the
    // given function with one that maps a failure from a String to a
    // ValidationError
    val fieldValidator = validator andThen (_.bimap((e:String) => Validators.formatFailure(field,e), identity))

    ValueValidation(this, fieldValidator)
  }

}

case class ChainedBinder[A, AA <: TypedBinder[A],B, BB <: TypedBinder[B]](v: AA ~ BB) extends TypedBinder[A ~ B] {
  def field = v._1.field + ", " + v._2.field

  def validate = {
    //todo: this should be applicative not mondic so that we accumulate errors
    val va: Validation[ValidationError, A] = v._1.validate
    val vb: Validation[ValidationError, B] = v._2.validate
    for {  a <- va;
           b <- vb} yield(new ~(a, b))
  }

  override def errors: IndexedSeq[ValidationError] = v._1.errors ++ v._2.errors
}

/**
  A Binder which actually directly binds to a piece of data
  */
sealed trait ValueBinder[A] extends TypedBinder[A] {
  var bound : FieldValidation[A] = _

  final def validate: FieldValidation[A] = bound
}

/**
  A binder which binds to some other binder
  */
trait MapBinder[A, F <: Binder] extends TypedBinder[A] {
  def previous: F
  def field = previous.field
}

case class ValueMap[A, F <: Binder](val previous: F, transform: F#R=>A) extends MapBinder[A,F] {
  def validate: FieldValidation[A] = previous.validate map transform
}

case class ValueFlatMap[A, F <: Binder](val previous: F, transform: F#R=>TypedBinder[A]) extends MapBinder[A,F] {
  def validate: FieldValidation[A] = {
    previous.validate.fold(_.fail, transform(_).validate)
  }
}

case class ValueValidation[A, F <: TypedBinder[A]](previous: F, validator: FieldValidator[A]) extends MapBinder[A,F] {
  def validate: FieldValidation[A] = previous.validate flatMap validator
}

case class HeaderBinder[A](field: String) extends ValueBinder[A] 
case class BodyBinder[A](field: String) extends ValueBinder[A]  
case class QueryBinder[A](field: String) extends ValueBinder[A]
