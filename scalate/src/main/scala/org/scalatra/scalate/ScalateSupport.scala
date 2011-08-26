package org.scalatra
package scalate

import java.io.PrintWriter
import javax.servlet.{ServletContext, ServletConfig, FilterConfig}
import javax.servlet.http.{HttpServletResponse, HttpServletRequest}
import org.fusesource.scalate.{TemplateEngine, Binding}
import org.fusesource.scalate.layout.DefaultLayoutStrategy
import org.fusesource.scalate.servlet.{ServletRenderContext, ServletTemplateEngine}
import org.fusesource.scalate.support.TemplateFinder

object ScalateSupport {
  val DefaultLayouts = Seq(
    "/WEB-INF/layouts/default",
    "/WEB-INF/scalate/layouts/default"
  )
  private def setLayoutStrategy(engine: TemplateEngine) = {
    val layouts = for {
      base <- ScalateSupport.DefaultLayouts
      extension <- TemplateEngine.templateTypes
    } yield ("%s.%s".format(base, extension))
    engine.layoutStrategy = new DefaultLayoutStrategy(engine, layouts:_*)
  }
}

trait ScalateSupport extends ScalatraKernel {
  protected var templateEngine: TemplateEngine = _

  abstract override def initialize(config: Config) {
    super.initialize(config)
    templateEngine = createTemplateEngine(config)
  }

  protected def createTemplateEngine(config: Config) = {
    val engine = config match {
      case servletConfig: ServletConfig =>
        new ServletTemplateEngine(servletConfig) with ScalatraTemplateEngine
      case filterConfig: FilterConfig =>
        new ServletTemplateEngine(filterConfig) with ScalatraTemplateEngine
      case _ =>
        // Don't know how to convert your Config to something that
        // ServletTemplateEngine can accept, so fall back to a TemplateEngine
        new TemplateEngine with ScalatraTemplateEngine
    }
    configureTemplateEngine(engine)
  }

  protected def configureTemplateEngine(engine: TemplateEngine) = engine

  /**
   * A TemplateEngine integrated with Scalatra.
   *
   * A ScalatraTemplateEngine looks for layouts in `/WEB-INF/layouts` before
   * searching the default `/WEB-INF/scalate/layouts`.
   */
  trait ScalatraTemplateEngine {
    this: TemplateEngine =>

    /**
     * Returns a ServletRenderContext constructed from the current
     * request and response.
     */
    override def createRenderContext(uri: String, out: PrintWriter) =
      ScalateSupport.this.createRenderContext

    /**
     * Delegates to the ScalatraKernel's isDevelopmentMode flag.
     */
    override def isDevelopmentMode = ScalateSupport.this.isDevelopmentMode

    ScalateSupport.setLayoutStrategy(this)

    templateDirectories = defaultTemplatePath
  }

  def createRenderContext: ServletRenderContext =
    createRenderContext(request, response)

  def createRenderContext(req: HttpServletRequest, resp: HttpServletResponse): ServletRenderContext = {
    val context = new ServletRenderContext(templateEngine, req, resp, servletContext)
    configureRenderContext(context)
  }

  protected def configureRenderContext(context: ServletRenderContext) = context

  def renderTemplate(path: String, attributes: (String, Any)*) =
    createRenderContext.render(path, Map(attributes : _*))

  /**
   * Flag whether the Scalate error page is enabled.  If true, uncaught
   * exceptions will be caught and rendered by the Scalate error page.
   *
   * The default is true.
   */
  protected def isScalateErrorPageEnabled = true

  abstract override def handle(req: HttpServletRequest, res: HttpServletResponse) {
    try {
      super.handle(req, res)
    }
    catch {
      case e if isScalateErrorPageEnabled => renderScalateErrorPage(req, res, e)
      case e => throw e
    }
  }

  // Hack: Have to pass it the request and response, because we're outside the
  // scope of the super handler.
  private def renderScalateErrorPage(req: HttpServletRequest, resp: HttpServletResponse, e: Throwable) = {
    resp.setContentType("text/html")
    val errorPage = templateEngine.load("/WEB-INF/scalate/errors/500.scaml")
    val context = createRenderContext(req, resp)
    context.setAttribute("javax.servlet.error.exception", Some(e))
    templateEngine.layout(errorPage, context)
  }

  /**
   * The default index page when the path is a directory.
   */
  protected def defaultIndexName: String = "index"

  /**
   * The default template format.
   */
  protected def defaultTemplateFormat: String = "scaml"

  /**
   * The default path to search for templates.  Left as a def so it can be
   * read from the servletContext in initialize, but you probably want a
   * constant.
   *
   * Defaults to:
   * - `/WEB-INF/views` (recommended)
   * - `/WEB-INF/scalate/templates` (used by previous Scalatra quickstarts)
   */
  protected def defaultTemplatePath: List[String] =
    List("/WEB-INF/views", "/WEB-INF/scalate/templates")

  /**
   * Convenience method for `layoutTemplateAs("jade")`.
   */
  protected def jade(path: String, attributes: (String, Any)*): String =
    renderTemplateAs("jade")(path, attributes:_*)

  /**
   * Convenience method for `layoutTemplateAs("scaml")`.
   */
  protected def scaml(path: String, attributes: (String, Any)*): String =
    renderTemplateAs("scaml")(path, attributes:_*)

  /**
   * Convenience method for `layoutTemplateAs("ssp")`.
   */
  protected def ssp(path: String, attributes: (String, Any)*): String =
    renderTemplateAs("ssp")(path, attributes:_*)

  /**
   * Convenience method for `layoutTemplateAs("mustache")`.
   */
  protected def mustache(path: String, attributes: (String, Any)*): String =
    renderTemplateAs("mustache")(path, attributes:_*)

  /**
   * Finds and renders a template with the current layout strategy,
   * returning the result.
   *
   * @param ext The extension to look for a template.
   * @param path The path of the template, passed to `findTemplate`.
   * @param attributes Attributes to path to the render context.  Disable
   * layouts by passing `layout -> ""`.
   */
  protected def renderTemplateAs(ext: String)(path: String, attributes: (String, Any)*): String = {
    val uri = findTemplate(path, Set(ext)).getOrElse(path)
    templateEngine.layout(uri, Map(attributes:_*))
  }

  /**
   * Finds a template for a path.  Delegates to a TemplateFinder, and if
   * that fails, tries again with `/defaultIndexName` appended.
   */
  protected def findTemplate(path: String, extensionSet: Set[String] = templateEngine.extensions): Option[String] = {
    val finder = new TemplateFinder(templateEngine) {
      override lazy val extensions = extensionSet
    }
    finder.findTemplate("/"+path) orElse
      finder.findTemplate("/%s/%s".format(path, defaultIndexName))
  }
}
