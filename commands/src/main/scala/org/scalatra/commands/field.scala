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

class BinderOps[A, S, V <: Binder[A]](v: V) {
  def bindTo(data: BodySource[S], params: MultiParams, headers: Map[String,String])(implicit binding: Binding[A,V,S]) : FieldValidation[A] = {
    binding.apply(v, data, params, headers)
    v.validate
  }
}

trait Binding[A, V <: Binder[A], S] {
  def apply(binder: V, data: BodySource[S], params: MultiParams, headers: Map[String,String]) : Unit
}

object Binding {
  implicit def chainedBinding[B, A, F <: Binder[B], S](implicit bbinding: Binding[B, F, S]) : Binding[A, ChainedBinder[B, A, F], S] = new Binding[A, ChainedBinder[B, A, F], S] {
    def apply(binder: ChainedBinder[B,A,F], data: BodySource[S], params: MultiParams, headers: Map[String,String]) = {
      bbinding.apply(binder.previous, data, params, headers)
    }
  }

  implicit def headerBinding[A, S](implicit tcf: TypeConverterFactory[A]): Binding[A,HeaderBinder[A],S] = new Binding[A,HeaderBinder[A],S] {
    def apply(binder: HeaderBinder[A], data: BodySource[S], params: MultiParams, headers: Map[String,String]) = {
      val tc: TypeConverter[String, A] = tcf.resolveStringParams
      val result = for {
        header <- headers.get(binder.field).toSuccess(NonEmptyList(ValidationError("%s is required".format(binder.field), binder.field.some, NotFound.some)))
        converted <- tc(header).toSuccess(NonEmptyList(ValidationError("error converting %s".format(binder.field), binder.field.some, ValidationFail.some)))
      } yield(converted)
      binder.bound = result
    }
  }

  implicit def bodyBinding[A, S](implicit r: S => ValueReader[S, A]): Binding[A,BodyBinder[A],S] = new Binding[A,BodyBinder[A],S] {
    def apply(binder: BodyBinder[A], data: BodySource[S], params: MultiParams, headers: Map[String,String]) = {
      val result = data.read(binder.field) match {
        case Left(e) => NonEmptyList(ValidationError("unknown error fetching %s".format(binder.field), binder.field.some, UnknownError.some)).fail
        case Right(None) => NonEmptyList(ValidationError("%s is required".format(binder.field), binder.field.some, NotFound.some)).fail
        case Right(Some(a)) => a.successNel
      }
      binder.bound = result
    }
  }

  implicit def queryBinding[A, S](implicit tcf: TypeConverterFactory[A], multiParams: MultiParams => ValueReader[MultiParams, Seq[String]]): Binding[A,QueryBinder[A],S] = new Binding[A,QueryBinder[A],S] {
    def apply(binder: QueryBinder[A], data: BodySource[S], params: MultiParams, headers: Map[String,String]) = {
      val result = params.read(binder.field) match {
        case Left(e) => NonEmptyList(ValidationError("unknown error fetching %s".format(binder.field), binder.field.some, UnknownError.some)).fail
        case Right(None) => NonEmptyList(ValidationError("%s is required".format(binder.field), binder.field.some, NotFound.some)).fail
        case Right(Some(a)) => tcf.resolveMultiParams(a).toSuccess(NonEmptyList(ValidationError("unknown error fetching %s".format(binder.field), binder.field.some, UnknownError.some)))
      }
      binder.bound = result
    }
  }
}

object Binder {
  implicit def toOps[A, S, V <: Binder[A]](b: V) : BinderOps[A,S,V] = new BinderOps(b)
}

sealed trait Binder[A] {
  /**
    Used to document which field caused a failure
    */
  def field : String

  /**
    Validate the value.
    this separateion allows us to have a two staged bind + validate.
    */
  def validate: FieldValidation[A]
  /**
    validate a new validation to this binder
    */
  def validateWith(validators: Validator[A]*) : Binder[A] = {
    // This could just be validators.suml if we accept a List[Validator[A]] instead of varargs, hmm
    // but anyway... collapses all the validators into a single validator which returns multiple possible failures.
    // which perhaps we don't want, we might perhaps just want the first failure
    val validator : Validator[A] = if(validators.isEmpty) Monoid[Validator[A]].zero else validators.tail.foldLeft(validators.head)(_ |+| _)

    // we ask for validators which fail with a string.  we compose the
    // given function with one that maps a failure from a String to a
    // ValidationError
    val fieldValidator = validator andThen (_.bimap(_.map((e:String) => Validators.formatFailure(field,e)), identity))

    ValueValidation(this, fieldValidator)
  }

  def map[B](f: A=>B): Binder[B] = ValueMap(this, f)
  def flatMap[B](f: A=> Binder[B]): Binder[B] = ValueFlatMap(this, f)
}

/**
  A Binder which actually directly binds to a piece of data
  */
sealed trait ValueBinder[A] extends Binder[A] {
  var bound : FieldValidation[A] = _
  final def validate: FieldValidation[A] = bound
}

/**
  A binder which binds to some other binder
  */
abstract class ChainedBinder[B, A, F <: Binder[B]](val previous: F) extends Binder[A] {
  def field = previous.field
}

case class ValueMap[B,A, F <: Binder[B]](previous: F, transform: B=>A) extends ChainedBinder[B,A,F](previous) 
{
  def validate: FieldValidation[A] = previous.validate map transform
}

case class ValueFlatMap[B, A, F <: Binder[B]](previous: F, transform: B=>Binder[A]) extends ChainedBinder[B,A,F](previous) 
{
  def validate: FieldValidation[A] = {
    previous.validate.fold(_.fail, transform(_).validate)
  }
}

case class ValueValidation[A, F <: Binder[A]](previous: F, validator: FieldValidator[A]) extends ChainedBinder[A,A,F](previous) {
  def validate: FieldValidation[A] = previous.validate flatMap validator
}

case class HeaderBinder[A](field: String) extends ValueBinder[A] 
case class BodyBinder[A](field: String) extends ValueBinder[A]  
case class QueryBinder[A](field: String) extends ValueBinder[A]


/*
object FieldDescriptor {
  def apply[T](name: String)(implicit mf: Manifest[T], defV: DefaultValue[T]): FieldDescriptor[T] = 
    new BasicFieldDescriptor[T](name, transformations = identity, defVal = DefVal(defV.default))
}
trait FieldDescriptor[T] {

  def name: String
  def value: FieldValidation[T]
  def validator: Option[Validator[T]]
  def notes: String 
  def notes(note: String): FieldDescriptor[T]
  def description: String 
  def description(desc: String): FieldDescriptor[T]
  def valueManifest: Manifest[T]
  def valueSource: ValueSource.Value
  def sourcedFrom(valueSource: ValueSource.Value): FieldDescriptor[T]
  def allowableValues: List[T]
  def allowableValues(vals: T*): FieldDescriptor[T]
  def displayName: Option[String]
  def displayName(name: String): FieldDescriptor[T]
  
  private[commands] def defVal: DefVal[T]
  def defaultValue: T = defVal.value
  def withDefaultValue(default: => T): FieldDescriptor[T] 

  def isValid = value.isSuccess
  def isInvalid = value.isFailure

  private[commands] def isRequired: Boolean
  def required: FieldDescriptor[T]
  def optional: FieldDescriptor[T]

  override def toString() = "FieldDescriptor(name: %s)".format(name)

  def validateWith(validators: List[Validator[T]]): FieldDescriptor[T]

  def apply[S](original: Either[String, Option[S]])(implicit ms: Manifest[S], df: DefaultValue[S], convert: TypeConverter[S, T]): DataboundFieldDescriptor[S, T]

  override def hashCode() = 41 + 41 * name.hashCode()

  def transform(endo: T => T): FieldDescriptor[T]

  private[commands] def transformations: T => T

  override def equals(obj: Any) = obj match {
    case b : FieldDescriptor[_] => b.name == this.name
    case _ => false
  }

}

class BasicFieldDescriptor[T](
    val name: String, 
    val validator: Option[Validator[T]] = None, 
    private[commands] val transformations: T => T = identity _,
    private[commands] var isRequired: Boolean = false,
    val description: String = "",
    val notes: String = "",
    private[commands] val defVal: DefVal[T],
    val valueSource: ValueSource.Value = ValueSource.Body,
    val allowableValues: List[T] = Nil,
    val displayName: Option[String] = None)(implicit val valueManifest: Manifest[T]) extends FieldDescriptor[T] {

  val value: FieldValidation[T] = defaultValue.success

  def validateWith(validators: List[Validator[T]]): FieldDescriptor[T] =
    copy(validator = validator |+| validators.suml.some)

  def copy(
      name: String = name, 
      validator: Option[Validator[T]] = validator, 
      transformations: T => T = transformations, 
      isRequired: Boolean = isRequired, 
      description: String = description, 
      notes: String = notes,
      defVal: DefVal[T] = defVal,
      valueSource: ValueSource.Value = valueSource,
      allowableValues: List[T] = allowableValues,
      displayName: Option[String] = displayName): FieldDescriptor[T] = {
    val b = this
    new BasicFieldDescriptor(name, validator, transformations, isRequired, description, notes, defVal, valueSource, allowableValues, displayName)(valueManifest) 
  }

  def apply[S](original: Either[String, Option[S]])(implicit ms: Manifest[S], df: DefaultValue[S], convert: TypeConverter[S, T]): DataboundFieldDescriptor[S, T] = {
    val defValS = df.default
    val conv = original.fold(Validators.formatFailure(name, _).failNel, o => (convert(o | defValS) | defaultValue).success)
    val o = original.fold(_ => defValS, og => og | defValS)
    BoundFieldDescriptor(o, conv, this)
  }

  def transform(endo: T => T): FieldDescriptor[T] = copy(transformations = transformations andThen endo)

  def required = copy(isRequired = true)

  def optional = copy(isRequired = false)
  
  def description(desc: String) = copy(description = desc)
  
  def notes(note: String) = copy(notes = note)
  
  def withDefaultValue(default: => T): FieldDescriptor[T] = copy(defVal = DefVal(default)) 
  
  def sourcedFrom(valueSource: ValueSource.Value): FieldDescriptor[T] = copy(valueSource = valueSource)

  def allowableValues(vals: T*): FieldDescriptor[T] = copy(allowableValues = vals.toList).validateWith(List(Validators.oneOf(vals:_*)))

  def displayName(name: String): FieldDescriptor[T] = copy(displayName = name.blankOption)
}


trait DataboundFieldDescriptor[S, T] extends FieldDescriptor[T] {
  def field: FieldDescriptor[T]
  def original: S
  def transform(endo: T => T): DataboundFieldDescriptor[S, T]
  def apply[V](original: Either[String, Option[V]])(implicit mv: Manifest[V], df: DefaultValue[V], convert: TypeConverter[V, T]): DataboundFieldDescriptor[V, T] =
    this.asInstanceOf[DataboundFieldDescriptor[V, T]]

  override def toString() = "FieldDescriptor(name: %s, original: %s, value: %s)".format(name, original, value)
  def validate: ValidatedFieldDescriptor[S, T]
  def validateWith(validators: List[Validator[T]]): DataboundFieldDescriptor[S, T]
  def required: DataboundFieldDescriptor[S, T]
  def optional: DataboundFieldDescriptor[S, T]
  def isRequired = field.isRequired
  def description = field.description
  def description(desc: String): DataboundFieldDescriptor[S, T] 
  def notes = field.notes
  def notes(note: String): DataboundFieldDescriptor[S, T]
  def valueManifest = field.valueManifest
  private[commands] def defVal: DefVal[T] = field.defVal
  def withDefaultValue(default: => T): DataboundFieldDescriptor[S, T] 
  def valueSource: ValueSource.Value = field.valueSource
  def sourcedFrom(valueSource: ValueSource.Value): DataboundFieldDescriptor[S, T]
  def allowableValues = field.allowableValues
  def allowableValues(vals: T*): DataboundFieldDescriptor[S, T]
  def displayName: Option[String] = field.displayName
  def displayName(name: String): DataboundFieldDescriptor[S, T]
  
}

trait ValidatedFieldDescriptor[S, T] extends DataboundFieldDescriptor[S, T] {
  def validate: ValidatedFieldDescriptor[S, T] = this
}

object BoundFieldDescriptor {
  def apply[S:DefaultValue, T](original: S, value: FieldValidation[T], binding: FieldDescriptor[T]): DataboundFieldDescriptor[S, T] =
    new BoundFieldDescriptor(original, value, binding, binding.validator)
}

class BoundFieldDescriptor[S:DefaultValue, T](
    val original: S, 
    val value: FieldValidation[T], 
    val field: FieldDescriptor[T], 
    val validator: Option[Validator[T]]) extends DataboundFieldDescriptor[S, T] {
  def name: String = field.name
  

  override def hashCode(): Int = field.hashCode()
  override def equals(other: Any) = other match {
    case o: BasicFieldDescriptor[T] => field.equals(o)
    case o: BoundFieldDescriptor[T, S] => field.equals(o.field)
    case _ => false
  }
  override def toString() = "BoundFieldDescriptor(name: %s, original: %s, converted: %s)".format(name, original, value)

  def validateWith(validators: List[Validator[T]]): DataboundFieldDescriptor[S, T] = {
    val nwFld = field.validateWith(validators)
    copy(field = nwFld, validator = nwFld.validator)
  }

  def copy(original: S = original, value: FieldValidation[T] = value, field: FieldDescriptor[T] = field, validator: Option[Validator[T]] = validator): DataboundFieldDescriptor[S, T] =
    new BoundFieldDescriptor(original, value, field, validator)

  def transform(endo: T => T): DataboundFieldDescriptor[S, T] = copy(value = value map endo)

  def required = copy(field = field.required)

  def optional = copy(field = field.optional)
  
  def description(desc: String) = copy(field = field.description(desc))
  
  def notes(note: String) = copy(field = field.notes(note))

  def validate: ValidatedFieldDescriptor[S, T] = {
    if (!isRequired && original == mdefault[S]) {
      new ValidatedBoundFieldDescriptor(value map transformations, this)
    } else {
      val doValidation: Option[Validator[T]] = isRequired.option({value: T => if(value != defaultValue) value.successNel else "%s is required.".failNel})

      val newValidation: Validator[T] = ~(validator |+| doValidation)
      val newValue = value.flatMap(newValidation andThen (_.bimap(_.map(Validators.formatFailure(name,_)), identity))).map(transformations)
      new ValidatedBoundFieldDescriptor(newValue, this)
    }
  }

  private[commands] def transformations: (T) => T = field.transformations
  
  def withDefaultValue(default: => T): DataboundFieldDescriptor[S, T] = copy(field = field.withDefaultValue(default))
  
  def sourcedFrom(valueSource: ValueSource.Value): DataboundFieldDescriptor[S, T] = copy(field = field.sourcedFrom(valueSource))

  def allowableValues(vals: T*): DataboundFieldDescriptor[S, T] = copy(field = field.allowableValues(vals:_*))

  def displayName(name: String): DataboundFieldDescriptor[S, T] = copy(field = field.displayName(name))
}

class ValidatedBoundFieldDescriptor[S, T](val value: FieldValidation[T], val field: DataboundFieldDescriptor[S, T]) extends ValidatedFieldDescriptor[S, T] {
  def name: String = field.name

  override def hashCode(): Int = field.hashCode()
  override def equals(other: Any) = other match {
    case o: BasicFieldDescriptor[T] => field.equals(o)
    case o: BoundFieldDescriptor[T, S] => field.equals(o.field)
    case o: ValidatedBoundFieldDescriptor[S, T] => field.equals(o.field)
    case _ => false
  }
  override def toString() = "BoundFieldDescriptor(name: %s, original: %s, converted: %s)".format(name, original, value)

  def validateWith(validators: List[Validator[T]]): DataboundFieldDescriptor[S, T] = {
    copy(field = field.validateWith(validators))
  }

  def copy(value: FieldValidation[T] = value, field: DataboundFieldDescriptor[S, T] = field): ValidatedFieldDescriptor[S, T] =
    new ValidatedBoundFieldDescriptor(value, field)

  def transform(endo: T => T): DataboundFieldDescriptor[S, T] = copy(value = value map endo)

  def required = copy(field = field.required)

  def optional = copy(field = field.optional)
   
  def description(desc: String) = copy(field = field.description(desc))
  
  def notes(note: String) = copy(field = field.notes(note))

  def validator: Option[Validator[T]] = field.validator

  def original: S = field.original

  private[commands] def transformations: (T) => T = field.transformations
  
  def withDefaultValue(default: => T): DataboundFieldDescriptor[S, T] = copy(field = field.withDefaultValue(default))
  
  def sourcedFrom(valueSource: ValueSource.Value): DataboundFieldDescriptor[S, T] = copy(field = field.sourcedFrom(valueSource))
  
  def allowableValues(vals: T*): DataboundFieldDescriptor[S, T] = copy(field = field.allowableValues(vals:_*))
  
  def displayName(name: String): DataboundFieldDescriptor[S, T] = copy(field = field.displayName(name))
}

import scala.util.matching.Regex

class Field[A:Manifest](descr: FieldDescriptor[A], command: Command) {

  val name = descr.name
  def validation: FieldValidation[A] = binding.field.value.asInstanceOf[FieldValidation[A]]
  def value: Option[A] = binding.field.value.toOption.asInstanceOf[Option[A]]
  def defaultValue: A = descr.defaultValue
  def errors: List[ValidationError] = binding.field.value.fold(_.list, _=>List())
  def original = binding.original

  def binding: Binding = command.bindings(name)

  def isValid = validation.isSuccess
  def isInvalid = validation.isFailure
  
	def notes: String = descr.notes
  def description: String = descr.description
  
  def isRequired: Boolean = descr.isRequired
  def valueSource: ValueBinder.Value = descr.valueSource
  def allowableValues = descr.allowableValues
  def displayName: Option[String] = descr.displayName
}
 */
