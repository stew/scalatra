package org.scalatra
package commands

import org.json4s._
import json.{ NativeJsonSupport, NativeJsonValueReaderProperty }
import grizzled.slf4j.Logger

trait NativeJsonParsing extends CommandSupport with NativeJsonValueReaderProperty { self: NativeJsonSupport with CommandSupport =>
/*
  override protected def bindCommand[T <: Command](newCommand: T)(implicit m: Manifest[T], jbinding: Binding[newCommand.fields.R, JValue], pbinding: Binding[newCommand.fields.R, Params]): T = {
    format match {
      case "json" | "xml" => newCommand.bindTo(BodySource(parsedBody), multiParams, request.headers)(jbinding)
      case _ => newCommand.bindTo(BodySource(params), multiParams, request.headers)(pbinding)
    }
    requestProxy.update(commandRequestKey[T], newCommand)
    newCommand
  }
 */
}
