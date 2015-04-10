package org.obl.raz

import scalaz.{-\/, \/, \/-}
import exceptions._

import scala.language.higherKinds 
import scala.language.implicitConversions 

trait PathDecoder[T] {
  import PathDecoder.{Result}
  
  type Decoder[T] <: PathDecoder[T]
  def decode(path:Path):Result[T] 
  
  def decodeFull(path:Path):Throwable \/ T  = {
    decode(path) match {
      case -\/(err) => -\/(err)
      case \/-(pmr) if pmr.rest.isEmpty => \/-(pmr.value)
      case \/-(pmr) => -\/(PathExpectationException(BasePath.empty, pmr.rest))
    }
  }
  
  protected def createDecoder[T1](f:Path => Result[T1]):Decoder[T1]
  
  def map[T1](f:T => T1):Decoder[T1] = createDecoder[T1] { i:Path =>
    decode(i).map(_.mapValue(f))
  }
  
  def andThen[T1](f:PathMatchResult[T,Path] => Result[T1]):Decoder[T1] = 
    createDecoder { p => 
      decode(p).flatMap(f)
    }
  
  def flatMap[T1](f:T => PathDecoder[T1]):Decoder[T1]  = createDecoder[T1] { i:Path =>
    decode(i).flatMap { pmr =>
      f(pmr.value).decode(pmr.rest)
    }
  }

  def flatMapResult[T1](f:T => Throwable \/ T1):Decoder[T1] = {
    createDecoder( p => decode(p).flatMap { pmr =>
      f(pmr.value).map { v => PathMatchResult( v, pmr.rest) }
    })
  }
  
  def orElse[ST >: T, T1 <: ST](d: => PathDecoder[T1]):PathDecoder[ST] = createDecoder[ST] { pth =>
    decode(pth) match {
      case \/-(r) => \/-(r)
      case _ => d.decode(pth)
    }
  }
  
  def unapply[U](p:U)(implicit extPathDecode:ext.ExtPathDecode[U]):Option[T] = {
    this.decodeFull(extPathDecode(p)).toOption
  }
  
  lazy val Partial = PathDecoder.partialUnapply(decode(_))
  
}

object PathDecoder {
  
  type Result[T] = Throwable \/ PathMatchResult[T,Path] 
  
  def apply[T](f:Path=> Result[T]):PathDecoder[T] =
    new PathDecoder[T] {
        type Decoder[T] = PathDecoder[T]
        def decode(path:Path):Result[T] = f(path) 
        def createDecoder[T1](f:Path => Result[T1]):Decoder[T1] = PathDecoder.apply(f)
    }
  
  private def partialUnapply[T](f:Path=> Result[T]):PathDecoder[T] =
    new PathDecoder[T] {
        type Decoder[T] = PathDecoder[T]
        def decode(path:Path):Result[T] = f(path) 
        def createDecoder[T1](f:Path => Result[T1]):Decoder[T1] = PathDecoder.apply(f)
        override def unapply[U](p:U)(implicit extPathDecode:ext.ExtPathDecode[U]):Option[T] = {
          f(extPathDecode(p)).map(_.value).toOption
        }
    }
  
  def fromPath(p:Path):PathDecoder[Path] = apply[Path] { p1:Path =>
    PathUtils.subtract(p1,p).map { rest =>
      \/-(PathMatchResult(p, rest))
    }.getOrElse {
      -\/(new exceptions.PathExpectationException(p, p1))
    }
  }
  
  private[raz] def withSuffix[T](d:PathDecoder[T], sfx:Path) = {
    PathDecoder[T] { p =>
      d.decode(p).flatMap { pr =>
        PathUtils.subtract(pr.rest, sfx) match {
          case -\/(err) => -\/(err)
          case \/-(newRest) => \/-(PathMatchResult(pr.value, newRest))
        }
      }
    }
  }
  
  def seq[T](dec:PathDecoder[T]):PathDecoder[Seq[T]] = {
    PathDecoder[Seq[T]] { p:Path =>
      var pth = p
      val buf = collection.mutable.Buffer.empty[T]
      var finished = pth.isEmpty
      while (!finished) {
        val ri = dec.decode(pth)
        ri match {
          case -\/(_) => finished = true
          case \/-(v) => {
             buf += v.value
             pth = v.rest
             finished = pth.isEmpty
          }
        }
      }
      \/-(PathMatchResult(buf, pth))
    }
  }
  
  def opt[T](dec:PathDecoder[T]):PathDecoder[Option[T]] = {
    PathDecoder[Option[T]] { pth:Path =>
      val ri = dec.decode(pth)
      ri match {
        case -\/(_) => \/-(PathMatchResult(None, pth))
        case \/-(v) => \/-(PathMatchResult(Some(v.value), v.rest))
      }
    }
  }
  
  private def decodeUrl(str: String) = {
    java.net.URLDecoder.decode(str.replace("+", "%2B"), "UTF-8").replace("%2B", "+").trim
  }
  
  lazy val stringSegment = PathDecoder[String] { p:Path =>
    val pth = Path.ralativePath(p) //if (Path.baseOf(p).nonEmpty) Path.copy(p)(base = None) else p   
    val psgs = pth.path.path
    psgs.headOption match {
      case None => -\/(EndOfPathException)
      case Some(hd) => \/-(PathMatchResult(decodeUrl(hd), Path.copy(p)(path = PathSg(psgs.tail))))
    }
  }
  
  def fromStringSegment[T](f:String => T) = stringSegment.flatMapResult { x => \/.fromTryCatch(f(x)) }
  
  lazy val intSegment = fromStringSegment(_.toInt)
  lazy val longSegment = fromStringSegment(_.toLong)
  lazy val booleanSegment = fromStringSegment(_.toBoolean)
  
  def enumSegment[E <: Enumeration](e:E) = fromStringSegment(e.withName(_))
  
  lazy val optStringParam = PathDecoder[(String, Option[String])] { p:Path =>
    val pth = Path.ralativePath(p) //if (Path.baseOf(p).nonEmpty) Path.copy(p)(base = None) else p
    if (pth.params.nonEmpty) -\/(MissingPathPartException(PathPart.ParamsPart, p))
    else {
      val qgs = pth.params
      qgs.headOption match {
        case None => -\/(EndOfPathException)
        case Some(hd) => \/-(PathMatchResult(hd.name -> hd.value, Path.copy(p)(params = qgs.tail)))
      }  
    }
  }
  
  lazy val stringParam = PathDecoder[(String, String)] { p =>
    optStringParam.decode(p).flatMap {
      case PathMatchResult((nm:String, Some(v)), rest) => \/-(PathMatchResult((nm, v), rest))
      case PathMatchResult((nm:String, None), rest) => -\/(MissingPathPartException(PathPart.ParamValue(nm), rest))
    }
  }
  
  def fromStringParam[O](f:String => O) = stringParam.flatMapResult( p => \/.fromTryCatch(f(p._2)).map( p._1 -> _ ) )
  
  lazy val intParam = fromStringParam(_.toInt)
  lazy val longParam = fromStringParam(_.toLong)
  lazy val booleanParam = fromStringParam(_.toBoolean)
  
  def optStringParamValue(pred:QParamSg => Boolean, paramDescription:String) = PathDecoder[(String, Option[String])] { p:Path =>
    val pth = Path.ralativePath(p) //if (Path.baseOf(p).nonEmpty) Path.copy(p)(base = None) else p
    if (pth.params.isEmpty) -\/(MissingPathPartException(PathPart.ParamsPart, p))
    else {
       val rest = collection.mutable.Buffer.empty[QParamSg]
       var resPar:Option[QParamSg] = None
       p.params.foreach { par => 
         if (resPar.isEmpty && pred(par)) resPar = Some(par) 
         else rest += par
       }
       resPar match {
         case None => -\/(MissingPathPartException(PathPart.ParamValue(paramDescription), pth)) 
         case Some(resPar) => \/-(PathMatchResult(resPar.name -> resPar.value, Path.copy(p)(params = rest)))
       }
    }
  }
  
  def stringParamValue(pred:QParamSg => Boolean, paramDescription:String) = PathDecoder { p =>
    optStringParamValue(pred, paramDescription).decode(p).flatMap {
      case PathMatchResult((nm, Some(v)), rest) => \/-(PathMatchResult((nm, v), rest))
      case PathMatchResult((nm, _), rest) => -\/(MissingPathPartException(PathPart.ParamValue(nm), rest))
    }  
  }
  
  def named[T](parName:String, f:String => T):PathDecoder[T] = 
      stringParamValue(_.name == parName, s"named $parName").map(_._2).flatMapResult { x => \/.fromTryCatch(f(x)) }
  
  lazy val stringParamValue = named(_:String, i => i)
  lazy val intParamValue = named(_:String, _.toInt)
  lazy val longParamValue = named(_:String, _.toLong)
  lazy val booleanParamValue = named(_:String, _.toBoolean)
  
  def enumParamValue[E <: Enumeration](e:E):String => PathDecoder[E#Value] = named(_:String, e.withName(_))
  
  implicit def apply[H <: HPath, D](h: H)(implicit pathMatcher: PathMatcher[H, D]): PathDecoder[D] = {
    pathMatcher.decoder(h)
  }
}
