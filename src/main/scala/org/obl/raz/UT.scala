package org.obl.raz


/*
 * FIXME: Questa classe e' usata solo per effetuare il rendering di UriTemplate. Purtoppo per far cio ciene creato un  BasePath e su tale istanza e' invocato soltanto il metotdo
 * uriTemplatyeRender
 */
trait UT[H, Out] {

  def apply(h: H): Out

}

case class UTType[T]()

object UTType {

  implicit val boolean = UTType[Boolean]()
  implicit val int = UTType[Int]()
  implicit val long = UTType[Long]()
  implicit val string = UTType[String]()

  implicit def opt[T](implicit it: UTType[T]) = new UTType[Option[T]]()
  implicit def seq[T](implicit it: UTType[T]) = new UTType[Seq[T]]()

}

object UT {

  import HPaths._

  private val utf = UriTemplateFactory
  type Root = HPathNil[_, _, _]

  def apply[H <: HPath, Out](f: H => Out) =
    new UT[H, Out] {
      def apply(h: H): Out = f(h)
    }

  implicit def utRoot[R <: RelativePathAspect, A <: CanAddAspect, P <: CanHavePrefixAspect] = {
    apply[HPathNil[R, A, P], HPathNil[R, A, P]] { h => h }
  }
    
  implicit def utn[H <: HPath, R <: RelativePathAspect, A <: CanAddAspect, P <: CanHavePrefixAspect, T1, Out <: HPath](implicit uttype: UTType[T1], preUt: UT[H, Out]) =
    apply[HPathCons[H, R, A, P, T1], HPathCons[Out, R, A, P, String]] { h =>
      HPathConsFactory[R, A, P].create[Out, String](preUt(h.head), PathF(utf.expansion(h.value), utf.utMatcher(h.value), h.value.suffix, h.value.expansionKind))
    }

}