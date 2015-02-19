package org.obl.raz

import scalaz.{-\/, \/, \/-}

trait PathMatcher[-H, T] {
	def decoder(h: H): PathDecoder[T]
}

object PathMatcher {

  import DecHPaths._
  
  def apply[H, T](f: H => PathDecoder[T]) =
    new PathMatcher[H, T] {
      def decoder(h: H) = f(h)
    }

  implicit def matcher1[T1] = apply[DecHPath1[T1], T1]( { h =>
    PathDecoder[T1] { pth =>
      PathUtils.subtract(pth, h.head.path).flatMap(r0 => h.value.decode(r0))
    } 
  })
  
//  implicit def matcher2[T1,T2] = apply[DecHPath2[T1,T2], (T1,T2)]( { h =>
//    matcher1.decoder(h.head).flatMap { pmr =>
//      h.value.decode(pmr.rest).map( res => res.mapValue { t2:T2 => (pmr.value, t2) })
//    }
//  })
  

  implicit def matcher2[T1,T2] = apply[DecHPath2[T1,T2], (T1,T2)]( { h =>
    matcher1[T1].decoder(h.head).andThen { pmr =>
      h.value.decode(pmr.rest).map( res => res.mapValue { v:T2 => (pmr.value, v) })
    }
  })

  implicit def matcher3[T1,T2,T3] = apply[DecHPath3[T1,T2,T3], (T1,T2,T3)]( { h =>
    matcher2[T1,T2].decoder(h.head).andThen { pmr =>
      h.value.decode(pmr.rest).map( res => res.mapValue { v:T3 => (pmr.value._1, pmr.value._2, v) })
    }
  })

  implicit def matcher4[T1,T2,T3,T4] = apply[DecHPath4[T1,T2,T3,T4], (T1,T2,T3,T4)]( { h =>
    matcher3[T1,T2,T3].decoder(h.head).andThen { pmr =>
      h.value.decode(pmr.rest).map( res => res.mapValue { v:T4 => (pmr.value._1, pmr.value._2, pmr.value._3, v) })
    }
  })

  implicit def matcher5[T1,T2,T3,T4,T5] = apply[DecHPath5[T1,T2,T3,T4,T5], (T1,T2,T3,T4,T5)]( { h =>
    matcher4[T1,T2,T3,T4].decoder(h.head).andThen { pmr =>
      h.value.decode(pmr.rest).map( res => res.mapValue { v:T5 => (pmr.value._1, pmr.value._2, pmr.value._3, pmr.value._4, v) })
    }
  })

  implicit def matcher6[T1,T2,T3,T4,T5,T6] = apply[DecHPath6[T1,T2,T3,T4,T5,T6], (T1,T2,T3,T4,T5,T6)]( { h =>
    matcher5[T1,T2,T3,T4,T5].decoder(h.head).andThen { pmr =>
      h.value.decode(pmr.rest).map( res => res.mapValue { v:T6 => (pmr.value._1, pmr.value._2, pmr.value._3, pmr.value._4, pmr.value._5, v) })
    }
  })

  implicit def matcher7[T1,T2,T3,T4,T5,T6,T7] = apply[DecHPath7[T1,T2,T3,T4,T5,T6,T7], (T1,T2,T3,T4,T5,T6,T7)]( { h =>
    matcher6[T1,T2,T3,T4,T5,T6].decoder(h.head).andThen { pmr =>
      h.value.decode(pmr.rest).map( res => res.mapValue { v:T7 => (pmr.value._1, pmr.value._2, pmr.value._3, pmr.value._4, pmr.value._5, pmr.value._6, v) })
    }
  })

  implicit def matcher8[T1,T2,T3,T4,T5,T6,T7,T8] = apply[DecHPath8[T1,T2,T3,T4,T5,T6,T7,T8], (T1,T2,T3,T4,T5,T6,T7,T8)]( { h =>
    matcher7[T1,T2,T3,T4,T5,T6,T7].decoder(h.head).andThen { pmr =>
      h.value.decode(pmr.rest).map( res => res.mapValue { v:T8 => (pmr.value._1, pmr.value._2, pmr.value._3, pmr.value._4, pmr.value._5, pmr.value._6, pmr.value._7, v) })
    }
  })

  implicit def matcher9[T1,T2,T3,T4,T5,T6,T7,T8,T9] = apply[DecHPath9[T1,T2,T3,T4,T5,T6,T7,T8,T9], (T1,T2,T3,T4,T5,T6,T7,T8,T9)]( { h =>
    matcher8[T1,T2,T3,T4,T5,T6,T7,T8].decoder(h.head).andThen { pmr =>
      h.value.decode(pmr.rest).map( res => res.mapValue { v:T9 => (pmr.value._1, pmr.value._2, pmr.value._3, pmr.value._4, pmr.value._5, pmr.value._6, pmr.value._7, pmr.value._8, v) })
    }
  })

  implicit def matcher10[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10] = apply[DecHPath10[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10], (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10)]( { h =>
    matcher9[T1,T2,T3,T4,T5,T6,T7,T8,T9].decoder(h.head).andThen { pmr =>
      h.value.decode(pmr.rest).map( res => res.mapValue { v:T10 => (pmr.value._1, pmr.value._2, pmr.value._3, pmr.value._4, pmr.value._5, pmr.value._6, pmr.value._7, pmr.value._8, pmr.value._9, v) })
    }
  })

  implicit def matcher11[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11] = apply[DecHPath11[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11], (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11)]( { h =>
    matcher10[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10].decoder(h.head).andThen { pmr =>
      h.value.decode(pmr.rest).map( res => res.mapValue { v:T11 => (pmr.value._1, pmr.value._2, pmr.value._3, pmr.value._4, pmr.value._5, pmr.value._6, pmr.value._7, pmr.value._8, pmr.value._9, pmr.value._10, v) })
    }
  })

  implicit def matcher12[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12] = apply[DecHPath12[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12], (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12)]( { h =>
    matcher11[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11].decoder(h.head).andThen { pmr =>
      h.value.decode(pmr.rest).map( res => res.mapValue { v:T12 => (pmr.value._1, pmr.value._2, pmr.value._3, pmr.value._4, pmr.value._5, pmr.value._6, pmr.value._7, pmr.value._8, pmr.value._9, pmr.value._10, pmr.value._11, v) })
    }
  })

  implicit def matcher13[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13] = apply[DecHPath13[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13], (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13)]( { h =>
    matcher12[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12].decoder(h.head).andThen { pmr =>
      h.value.decode(pmr.rest).map( res => res.mapValue { v:T13 => (pmr.value._1, pmr.value._2, pmr.value._3, pmr.value._4, pmr.value._5, pmr.value._6, pmr.value._7, pmr.value._8, pmr.value._9, pmr.value._10, pmr.value._11, pmr.value._12, v) })
    }
  })

  implicit def matcher14[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14] = apply[DecHPath14[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14], (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14)]( { h =>
    matcher13[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13].decoder(h.head).andThen { pmr =>
      h.value.decode(pmr.rest).map( res => res.mapValue { v:T14 => (pmr.value._1, pmr.value._2, pmr.value._3, pmr.value._4, pmr.value._5, pmr.value._6, pmr.value._7, pmr.value._8, pmr.value._9, pmr.value._10, pmr.value._11, pmr.value._12, pmr.value._13, v) })
    }
  })

  implicit def matcher15[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15] = apply[DecHPath15[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15], (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15)]( { h =>
    matcher14[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14].decoder(h.head).andThen { pmr =>
      h.value.decode(pmr.rest).map( res => res.mapValue { v:T15 => (pmr.value._1, pmr.value._2, pmr.value._3, pmr.value._4, pmr.value._5, pmr.value._6, pmr.value._7, pmr.value._8, pmr.value._9, pmr.value._10, pmr.value._11, pmr.value._12, pmr.value._13, pmr.value._14, v) })
    }
  })

  implicit def matcher16[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16] = apply[DecHPath16[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16], (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16)]( { h =>
    matcher15[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15].decoder(h.head).andThen { pmr =>
      h.value.decode(pmr.rest).map( res => res.mapValue { v:T16 => (pmr.value._1, pmr.value._2, pmr.value._3, pmr.value._4, pmr.value._5, pmr.value._6, pmr.value._7, pmr.value._8, pmr.value._9, pmr.value._10, pmr.value._11, pmr.value._12, pmr.value._13, pmr.value._14, pmr.value._15, v) })
    }
  })

  implicit def matcher17[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17] = apply[DecHPath17[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17], (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17)]( { h =>
    matcher16[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16].decoder(h.head).andThen { pmr =>
      h.value.decode(pmr.rest).map( res => res.mapValue { v:T17 => (pmr.value._1, pmr.value._2, pmr.value._3, pmr.value._4, pmr.value._5, pmr.value._6, pmr.value._7, pmr.value._8, pmr.value._9, pmr.value._10, pmr.value._11, pmr.value._12, pmr.value._13, pmr.value._14, pmr.value._15, pmr.value._16, v) })
    }
  })

  implicit def matcher18[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18] = apply[DecHPath18[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18], (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18)]( { h =>
    matcher17[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17].decoder(h.head).andThen { pmr =>
      h.value.decode(pmr.rest).map( res => res.mapValue { v:T18 => (pmr.value._1, pmr.value._2, pmr.value._3, pmr.value._4, pmr.value._5, pmr.value._6, pmr.value._7, pmr.value._8, pmr.value._9, pmr.value._10, pmr.value._11, pmr.value._12, pmr.value._13, pmr.value._14, pmr.value._15, pmr.value._16, pmr.value._17, v) })
    }
  })

  implicit def matcher19[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19] = apply[DecHPath19[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19], (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19)]( { h =>
    matcher18[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18].decoder(h.head).andThen { pmr =>
      h.value.decode(pmr.rest).map( res => res.mapValue { v:T19 => (pmr.value._1, pmr.value._2, pmr.value._3, pmr.value._4, pmr.value._5, pmr.value._6, pmr.value._7, pmr.value._8, pmr.value._9, pmr.value._10, pmr.value._11, pmr.value._12, pmr.value._13, pmr.value._14, pmr.value._15, pmr.value._16, pmr.value._17, pmr.value._18, v) })
    }
  })

  implicit def matcher20[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20] = apply[DecHPath20[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20], (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20)]( { h =>
    matcher19[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19].decoder(h.head).andThen { pmr =>
      h.value.decode(pmr.rest).map( res => res.mapValue { v:T20 => (pmr.value._1, pmr.value._2, pmr.value._3, pmr.value._4, pmr.value._5, pmr.value._6, pmr.value._7, pmr.value._8, pmr.value._9, pmr.value._10, pmr.value._11, pmr.value._12, pmr.value._13, pmr.value._14, pmr.value._15, pmr.value._16, pmr.value._17, pmr.value._18, pmr.value._19, v) })
    }
  })

  implicit def matcher21[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20,T21] = apply[DecHPath21[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20,T21], (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20,T21)]( { h =>
    matcher20[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20].decoder(h.head).andThen { pmr =>
      h.value.decode(pmr.rest).map( res => res.mapValue { v:T21 => (pmr.value._1, pmr.value._2, pmr.value._3, pmr.value._4, pmr.value._5, pmr.value._6, pmr.value._7, pmr.value._8, pmr.value._9, pmr.value._10, pmr.value._11, pmr.value._12, pmr.value._13, pmr.value._14, pmr.value._15, pmr.value._16, pmr.value._17, pmr.value._18, pmr.value._19, pmr.value._20, v) })
    }
  })

  implicit def matcher22[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20,T21,T22] = apply[DecHPath22[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20,T21,T22], (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20,T21,T22)]( { h =>
    matcher21[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20,T21].decoder(h.head).andThen { pmr =>
      h.value.decode(pmr.rest).map( res => res.mapValue { v:T22 => (pmr.value._1, pmr.value._2, pmr.value._3, pmr.value._4, pmr.value._5, pmr.value._6, pmr.value._7, pmr.value._8, pmr.value._9, pmr.value._10, pmr.value._11, pmr.value._12, pmr.value._13, pmr.value._14, pmr.value._15, pmr.value._16, pmr.value._17, pmr.value._18, pmr.value._19, pmr.value._20, pmr.value._21, v) })
    }
  })

  
}