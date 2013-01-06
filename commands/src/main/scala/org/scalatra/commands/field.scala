package org.scalatra
package commands

import validation._
import util.conversion._
import scalaz._
import Scalaz._
import mojolly.inflector.InflectorImports._
import org.scalatra.util.RicherString._
import validation._

object DefVal {
  def apply[T:Manifest](prov: => T) = new DefVal(prov)
}
class DefVal[T:Manifest](valueProvider: => T) {
  lazy val value = valueProvider
}
object ValueSource extends Enumeration {
  val Header = Value("header")
  val Body = Value("body")
  val Query = Value("query")
  val Path = Value("path")
}

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
  def valueSource: ValueSource.Value = descr.valueSource
  def allowableValues = descr.allowableValues
  def displayName: Option[String] = descr.displayName
}
