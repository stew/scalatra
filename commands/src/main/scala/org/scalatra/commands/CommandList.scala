package org.scalatra
package commands

sealed trait BindingList {
  def validate : Unit
  def bindTo[S](data: BodySource[S], params: MultiParams, headers: Map[String,String])(implicit binding: ListBinding[this.type, S]) = {
    binding(this, data, params, headers)
  }
}

trait BNil extends BindingList {
  def ::[H](h: Binder[H]) = commands.::[H, BNil](h, this)
  def validate { }
}

final case class ::[H, T <: BindingList](head: Binder[H], tail: T) extends BindingList {
  def validate {
    head.validate
    tail.validate
  }

  def ::[HH](h : Binder[HH]) : HH :: H :: T = commands.::(h, this)
}

trait ListBinding[L <: BindingList, S] {
  def apply(l: L, data: BodySource[S], params: MultiParams, headers: Map[String,String]) : Unit
}

object ListBinding {
  implicit def nilBinding[S] : ListBinding[BNil,S] = new ListBinding[BNil, S] {
    def apply(l: BNil, data: BodySource[S], params: MultiParams, headers: Map[String,String]) {}
  }
  implicit def someBinding[H, T <: BindingList, S](implicit restBinding: ListBinding[T, S], binding: Binding[_,_,_]) : ListBinding[H :: T, S] = new ListBinding[H :: T, S] {
    def apply(l: H :: T, data: BodySource[S], params: MultiParams, headers: Map[String,String]) {
      l.head.bindTo(data, params, headers)(binding)
      restBinding.apply(l.tail,data, params, headers)
    }
    
  }
}

final case object BNil extends BNil



