package org.scalatra
package commands

import org.json4s._
import json.{JacksonJsonValueReaderProperty, JacksonJsonSupport}
import grizzled.slf4j.Logger

trait JacksonJsonParsing extends CommandSupport with JacksonJsonValueReaderProperty { self: JacksonJsonSupport with CommandSupport =>
/* stew: TODO
 I don't know why this doesn't appear to be a valid override except, of course, it is the path dependant type

  override def bindCommand[T <: Command](newCommand: T)(implicit mf: Manifest[T], jbinding: Binding[newCommand.fields.R, JValue], pbinding: Binding[newCommand.fields.R, Params]): T = {
    format match {
      case "json" | "xml" => newCommand.bindTo(BodySource(parsedBody), multiParams, request.headers)(jbinding)
      case _ => newCommand.bindTo(BodySource(params), multiParams, request.headers)(pbinding)
    }
    requestProxy.update(commandRequestKey[T], newCommand)
    newCommand
  }
 */
}
