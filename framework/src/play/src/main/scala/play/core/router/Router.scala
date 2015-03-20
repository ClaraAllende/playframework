/*
 * Copyright (C) 2009-2013 Typesafe Inc. <http://www.typesafe.com>
 */
package play.core

import play.api.mvc._
import org.apache.commons.lang3.reflect.MethodUtils

import java.net.URI
import scala.util.Either.RightProjection
import scala.util.control.{ NonFatal, Exception }
import play.core.j.JavaActionAnnotations
import play.utils.UriEncoding
import play.api.Plugin

trait PathPart

case class DynamicPart(name: String, constraint: String, encodeable: Boolean) extends PathPart {
  override def toString = """DynamicPart("""" + name + "\", \"\"\"" + constraint + "\"\"\")" // "
}

case class StaticPart(value: String) extends PathPart {
  override def toString = """StaticPart("""" + value + """")"""
}

case class PathPattern(parts: Seq[PathPart]) {

  import java.util.regex._

  private def decodeIfEncoded(decode: Boolean, groupCount: Int): Matcher => Either[Throwable, String] = matcher =>
    Exception.allCatch[String].either {
      if (decode) {
        val group = matcher.group(groupCount)
        // If param is not correctly encoded, get path will return null, so we prepend a / to it
        new URI("/" + group).getPath.drop(1)
      } else
        matcher.group(groupCount)
    }

  lazy val (regex, groups) = {
    Some(parts.foldLeft("", Map.empty[String, Matcher => Either[Throwable, String]], 0) { (s, e) =>
      e match {
        case StaticPart(p) => ((s._1 + Pattern.quote(p)), s._2, s._3)
        case DynamicPart(k, r, encodeable) => {
          ((s._1 + "(" + r + ")"),
            (s._2 + (k -> decodeIfEncoded(encodeable, s._3 + 1))),
            s._3 + 1 + Pattern.compile(r).matcher("").groupCount)
        }
      }
    }).map {
      case (r, g, _) => Pattern.compile("^" + r + "$") -> g
    }.get
  }

  def apply(path: String): Option[Map[String, Either[Throwable, String]]] = {
    val matcher = regex.matcher(path)
    if (matcher.matches) {
      Some(groups.map {
        case (name, g) => name -> g(matcher)
      }.toMap)
    } else {
      None
    }
  }

  override def toString = parts.map {
    case DynamicPart(name, constraint, _) => "$" + name + "<" + constraint + ">"
    case StaticPart(path) => path
  }.mkString

}

/**
 * provides Play's router implementation
 */
object Router {

  object Route {

    trait ParamsExtractor {
      def unapply(request: RequestHeader): Option[RouteParams]
    }
    def apply(method: String, pathPattern: PathPattern) = new ParamsExtractor {

      def unapply(request: RequestHeader): Option[RouteParams] = {
        if (method == request.method) {
          pathPattern(request.path).map { groups =>
            RouteParams(groups, request.queryString)
          }
        } else {
          None
        }
      }

    }

  }

  object Include {

    def apply(router: Router.Routes) = new {

      def unapply(request: RequestHeader): Option[Handler] = {
        router.routes.lift(request)
      }

    }

  }

  /**
   * The context for a reverse route.
   *
   * This is made available implicitly to PathBindables and QueryStringBindables in the reverse router so that they can
   * query the fixed params that are passed to the action.
   *
   * An empty reverse router context is made available implicitly to the router and JavaScript router.
   *
   * @param fixedParams The fixed params that this route passes to the action.
   */
  case class ReverseRouteContext(fixedParams: Map[String, Any])

  object ReverseRouteContext {
    implicit val empty = ReverseRouteContext(Map())
  }

  case class JavascriptReverseRoute(name: String, f: String)

  case class Param[T](name: String, value: Either[String, T])

  case class RouteParams(path: Map[String, Either[Throwable, String]], queryString: Map[String, Seq[String]]) {

    def fromPath[T](key: String, default: Option[T] = None)(implicit binder: PathBindable[T]): Param[T] = {
      Param(key, path.get(key).map(v => v.fold(t => Left(t.getMessage), binder.bind(key, _))).getOrElse {
        default.map(d => Right(d)).getOrElse(Left("Missing parameter: " + key))
      })
    }

    def fromQuery[T](key: String, default: Option[T] = None)(implicit binder: QueryStringBindable[T]): Param[T] = {
      Param(key, binder.bind(key, queryString).getOrElse {
        default.map(d => Right(d)).getOrElse(Left("Missing parameter: " + key))
      })
    }

  }

  /**
   * Information about a `Handler`, especially useful for loading the handler
   * with reflection.
   */
  case class HandlerDef(classLoader: ClassLoader, routerPackage: String, controller: String, method: String, parameterTypes: Seq[Class[_]], verb: String, comments: String, path: String)

  def dynamicString(dynamic: String): String = {
    UriEncoding.encodePathSegment(dynamic, "utf-8")
  }

  def queryString(items: List[Option[String]]) = {
    Option(items.filter(_.isDefined).map(_.get).filterNot(_.isEmpty)).filterNot(_.isEmpty).map("?" + _.mkString("&")).getOrElse("")
  }

  // HandlerInvoker

  /**
   * An object that creates a `HandlerInvoker`. Used by the `createInvoker` method
   * to create a `HandlerInvoker` for each route. The `Routes.createInvoker` method looks
   * for an implicit `HandlerInvokerFactory` and uses that to create a `HandlerInvoker`.
   */
  @scala.annotation.implicitNotFound("Cannot use a method returning ${T} as a Handler for requests")
  trait HandlerInvokerFactory[T] {
    /**
     * Create an invoker for the given thunk that is never called.
     * @param fakeCall A simulated call to the controller method. Needed to
     * so implicit resolution can use the controller method's return type,
     * but *never actually called*.
     */
    def createInvoker(fakeCall: => T, handlerDef: HandlerDef): HandlerInvoker[T]
  }

  /**
   * An object that, when invoked with a thunk, produces a `Handler` that wraps
   * that thunk. Constructed by a `HandlerInvokerFactory`.
   */
  trait HandlerInvoker[T] {
    /**
     * Create a `Handler` that wraps the given thunk. The thunk won't be called
     * until the `Handler` is applied. The returned Handler will be used by
     * Play to service the request.
     */
    def call(call: => T): Handler
  }

  private def handlerTags(handlerDef: HandlerDef): Map[String, String] = Map(
    play.api.Routes.ROUTE_PATTERN -> handlerDef.path,
    play.api.Routes.ROUTE_VERB -> handlerDef.verb,
    play.api.Routes.ROUTE_CONTROLLER -> handlerDef.controller,
    play.api.Routes.ROUTE_ACTION_METHOD -> handlerDef.method,
    play.api.Routes.ROUTE_COMMENTS -> handlerDef.comments
  )

  private def taggedRequest(rh: RequestHeader, tags: Map[String, String]): RequestHeader = {
    val newTags = if (rh.tags.isEmpty) tags else rh.tags ++ tags
    rh.copy(tags = newTags)
  }

  object HandlerInvokerFactory {

    import play.libs.F.{ Promise => JPromise }
    import play.mvc.{ SimpleResult => JSimpleResult, Result => JResult, WebSocket => JWebSocket }
    import play.core.j.JavaWebSocket
    import com.fasterxml.jackson.databind.JsonNode

    /**
     * Create a `HandlerInvokerFactory` for a call that already produces a
     * `Handler`.
     */
    implicit def passThrough[A <: Handler]: HandlerInvokerFactory[A] = new HandlerInvokerFactory[A] {
      def createInvoker(fakeCall: => A, handlerDef: HandlerDef) = new HandlerInvoker[A] {
        def call(call: => A) = call
      }
    }

    private def loadJavaControllerClass(handlerDef: HandlerDef): Class[_] = {
      try {
        handlerDef.classLoader.loadClass(handlerDef.controller)
      } catch {
        case e: ClassNotFoundException =>
          // Try looking up relative to the routers package name.
          // This was primarily implemented for the documentation project so that routers could be namespaced and so
          // they could reference controllers relative to their own package.
          if (handlerDef.routerPackage.length > 0) {
            try {
              handlerDef.classLoader.loadClass(handlerDef.routerPackage + "." + handlerDef.controller)
            } catch {
              case NonFatal(_) => throw e
            }
          } else throw e
      }
    }

    /**
     * Create a `HandlerInvokerFactory` for a Java action. Caches the
     * tags and annotations.
     */
    private abstract class JavaActionInvokerFactory[A] extends HandlerInvokerFactory[A] {
      def createInvoker(fakeCall: => A, handlerDef: HandlerDef): HandlerInvoker[A] = new HandlerInvoker[A] {
        val cachedHandlerTags = handlerTags(handlerDef)
        val cachedAnnotations = {
          val controller = loadJavaControllerClass(handlerDef)
          val method = MethodUtils.getMatchingAccessibleMethod(controller, handlerDef.method, handlerDef.parameterTypes: _*)
          new JavaActionAnnotations(controller, method)
        }
        def call(call: => A): Handler = {
          new play.core.j.JavaAction with RequestTaggingHandler {
            val annotations = cachedAnnotations
            val parser = cachedAnnotations.parser
            def invocation: JPromise[JResult] = resultCall(call)
            def tagRequest(rh: RequestHeader) = taggedRequest(rh, cachedHandlerTags)
          }
        }
      }
      def resultCall(call: => A): JPromise[JResult]
    }

    implicit def wrapJava: HandlerInvokerFactory[JResult] = new JavaActionInvokerFactory[JResult] {
      def resultCall(call: => JResult) = JPromise.pure(call)
    }
    implicit def wrapJavaPromise: HandlerInvokerFactory[JPromise[JResult]] = new JavaActionInvokerFactory[JPromise[JResult]] {
      def resultCall(call: => JPromise[JResult]) = call
    }

    /**
     * Create a `HandlerInvokerFactory` for a Java WebSocket.
     */
    private abstract class JavaWebSocketInvokerFactory[A, B] extends HandlerInvokerFactory[A] {
      def webSocketCall(call: => A): WebSocket[B, B]
      def createInvoker(fakeCall: => A, handlerDef: HandlerDef): HandlerInvoker[A] = new HandlerInvoker[A] {
        val cachedHandlerTags = handlerTags(handlerDef)
        def call(call: => A): WebSocket[B, B] = webSocketCall(call)
      }
    }

    implicit def javaBytesWebSocket: HandlerInvokerFactory[JWebSocket[Array[Byte]]] = new JavaWebSocketInvokerFactory[JWebSocket[Array[Byte]], Array[Byte]] {
      def webSocketCall(call: => JWebSocket[Array[Byte]]) = JavaWebSocket.ofBytes(call)
    }

    implicit def javaStringWebSocket: HandlerInvokerFactory[JWebSocket[String]] = new JavaWebSocketInvokerFactory[JWebSocket[String], String] {
      def webSocketCall(call: => JWebSocket[String]) = JavaWebSocket.ofString(call)
    }

    implicit def javaJsonWebSocket: HandlerInvokerFactory[JWebSocket[JsonNode]] = new JavaWebSocketInvokerFactory[JWebSocket[JsonNode], JsonNode] {
      def webSocketCall(call: => JWebSocket[JsonNode]) = JavaWebSocket.ofJson(call)
    }

    implicit def javaBytesPromiseWebSocket: HandlerInvokerFactory[JPromise[JWebSocket[Array[Byte]]]] = new JavaWebSocketInvokerFactory[JPromise[JWebSocket[Array[Byte]]], Array[Byte]] {
      def webSocketCall(call: => JPromise[JWebSocket[Array[Byte]]]) = JavaWebSocket.promiseOfBytes(call)
    }

    implicit def javaStringPromiseWebSocket: HandlerInvokerFactory[JPromise[JWebSocket[String]]] = new JavaWebSocketInvokerFactory[JPromise[JWebSocket[String]], String] {
      def webSocketCall(call: => JPromise[JWebSocket[String]]) = JavaWebSocket.promiseOfString(call)
    }

    implicit def javaJsonPromiseWebSocket: HandlerInvokerFactory[JPromise[JWebSocket[JsonNode]]] = new JavaWebSocketInvokerFactory[JPromise[JWebSocket[JsonNode]], JsonNode] {
      def webSocketCall(call: => JPromise[JWebSocket[JsonNode]]) = JavaWebSocket.promiseOfJson(call)
    }
  }

  /**
   * A class that routes must extend. The routes compiler generates code that
   * extends this trait.
   */
  trait Routes {
    self =>

    def documentation: Seq[(String, String, String)]

    def routes: PartialFunction[RequestHeader, Handler]

    def setPrefix(prefix: String)

    def prefix: String

    def badRequest(error: String) = Action.async { request =>
      play.api.Play.maybeApplication.map(_.global.onBadRequest(request, error)).getOrElse(play.api.DefaultGlobal.onBadRequest(request, error))
    }

    def call(generator : => Handler) = generator

    def call[T](params: List[Param[_]])(generator: (Seq[_]) => Handler) : Handler = {
      (params.foldLeft[Either[String, Seq[_]]](Right(Seq[T]()))
      {(seq, param) => seq.right.flatMap(s => param.value.right.map(s :+ _))}).fold(badRequest, generator)
    }

    def handlerFor(request: RequestHeader): Option[Handler] = {
      routes.lift(request)
    }

    /**
     * An invoker that wraps another invoker, ensuring the request is tagged
     * appropriately.
     */
    private class TaggingInvoker[A](underlyingInvoker: HandlerInvoker[A], handlerDef: HandlerDef) extends HandlerInvoker[A] {
      val cachedHandlerTags = handlerTags(handlerDef)
      def call(call: => A): Handler = {
        val handler = underlyingInvoker.call(call)
        handler match {
          case alreadyTagged: RequestTaggingHandler => alreadyTagged
          case javaAction: play.core.j.JavaAction =>
            new play.core.j.JavaAction with RequestTaggingHandler {
              def invocation = javaAction.invocation
              val annotations = javaAction.annotations
              val parser = javaAction.annotations.parser
              def tagRequest(rh: RequestHeader) = taggedRequest(rh, cachedHandlerTags)
            }
          case action: EssentialAction => new EssentialAction with RequestTaggingHandler {
            def apply(rh: RequestHeader) = action(rh)
            def tagRequest(rh: RequestHeader) = taggedRequest(rh, cachedHandlerTags)
          }
          case ws @ WebSocket(f) => {
            WebSocket[ws.FramesIn, ws.FramesOut](rh => ws.f(taggedRequest(rh, cachedHandlerTags)))(ws.inFormatter, ws.outFormatter)
          }
          case handler => handler
        }
      }
    }

    def fakeValue[A]: A = throw new UnsupportedOperationException("Can't get a fake value")

    /**
     * Create a HandlerInvoker for a route by simulating a call to the
     * controller method. This method is called by the code-generated routes
     * files.
     */
    def createInvoker[T](
      fakeCall: => T,
      handlerDef: HandlerDef)(implicit hif: HandlerInvokerFactory[T]): HandlerInvoker[T] = {
      val underlyingInvoker = hif.createInvoker(fakeCall, handlerDef)
      new TaggingInvoker(underlyingInvoker, handlerDef)
    }

  }

}
