/*
 * Copyright (C) 2009-2015 Typesafe Inc. <http://www.typesafe.com>
 */
package play.core.routing

import play.api.http.HttpErrorHandler
import play.api.mvc._
import play.api.routing.Router

import play.utils.UriEncoding

/**
 * A route
 */
object Route {

  /**
   * Extractor of route from a request.
   */
  trait ParamsExtractor {
    def unapply(request: RequestHeader): Option[RouteParams]
  }

  /**
   * Create a params extractor from the given method and path pattern.
   */
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

/**
 * An included router
 */
class Include(val router: Router) {
  def unapply(request: RequestHeader): Option[Handler] = {
    router.routes.lift(request)
  }
}

/**
 * An included router
 */
object Include {
  def apply(router: Router) = new Include(router)
}

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

/**
 * A generated router.
 */
abstract class GeneratedRouter extends Router {

  def errorHandler: HttpErrorHandler

  def badRequest(error: String) = Action.async { request =>
    errorHandler.onClientError(request, play.api.http.Status.BAD_REQUEST, error)
  }

  def call(generator: => Handler): Handler = {
    generator
  }

  def call[T](params: List[Param[_]])(generator: (Seq[_]) => Handler): Handler =
    (params.foldLeft[Either[String, Seq[_]]](Right(Seq[T]())) { (seq, param) => seq.right.flatMap(s => param.value.right.map(s :+ _)) }).fold(badRequest, generator)

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

