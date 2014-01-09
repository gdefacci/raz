package org.obl.raz

trait BasePathMatchers {

  import HElems._

  def matcher1[T1](h: HElem1[T1]): Path => Option[PathMatchResult[T1, Path]] = { pth =>
    h.value.matchPath(pth)
  }

  def matcher2[T1, T2](h: HElem2[T1, T2]): Path => Option[PathMatchResult[(T1, T2), Path]] = { pth =>
    matcher1(h.head)(pth).flatMap { r0 =>
      h.value.matchPath(r0.rest).map { r =>
        PathMatchResult((r0.value, r.value), r.rest)
      }
    }
  }

  def matcher3[T1, T2, T3](h: HElem3[T1, T2, T3]): Path => Option[PathMatchResult[(T1, T2, T3), Path]] = { pth =>
    matcher2(h.head)(pth).flatMap { r0 =>
      h.value.matchPath(r0.rest).map { r =>
        PathMatchResult((r0.value._1, r0.value._2, r.value), r.rest)
      }
    }
  }

  def matcher4[T1, T2, T3, T4](h: HElem4[T1, T2, T3, T4]): Path => Option[PathMatchResult[(T1, T2, T3, T4), Path]] = { pth =>
    matcher3(h.head)(pth).flatMap { r0 =>
      h.value.matchPath(r0.rest).map { r =>
        PathMatchResult((r0.value._1, r0.value._2, r0.value._3, r.value), r.rest)
      }
    }
  }

  def matcher5[T1, T2, T3, T4, T5](h: HElem5[T1, T2, T3, T4, T5]): Path => Option[PathMatchResult[(T1, T2, T3, T4, T5), Path]] = { pth =>
    matcher4(h.head)(pth).flatMap { r0 =>
      h.value.matchPath(r0.rest).map { r =>
        PathMatchResult((r0.value._1, r0.value._2, r0.value._3, r0.value._4, r.value), r.rest)
      }
    }
  }

  def matcher6[T1, T2, T3, T4, T5, T6](h: HElem6[T1, T2, T3, T4, T5, T6]): Path => Option[PathMatchResult[(T1, T2, T3, T4, T5, T6), Path]] = { pth =>
    matcher5(h.head)(pth).flatMap { r0 =>
      h.value.matchPath(r0.rest).map { r =>
        PathMatchResult((r0.value._1, r0.value._2, r0.value._3, r0.value._4, r0.value._5, r.value), r.rest)
      }
    }
  }

  def matcher7[T1, T2, T3, T4, T5, T6, T7](h: HElem7[T1, T2, T3, T4, T5, T6, T7]): Path => Option[PathMatchResult[(T1, T2, T3, T4, T5, T6, T7), Path]] = { pth =>
    matcher6(h.head)(pth).flatMap { r0 =>
      h.value.matchPath(r0.rest).map { r =>
        PathMatchResult((r0.value._1, r0.value._2, r0.value._3, r0.value._4, r0.value._5, r0.value._6, r.value), r.rest)
      }
    }
  }

  def matcher8[T1, T2, T3, T4, T5, T6, T7, T8](h: HElem8[T1, T2, T3, T4, T5, T6, T7, T8]): Path => Option[PathMatchResult[(T1, T2, T3, T4, T5, T6, T7, T8), Path]] = { pth =>
    matcher7(h.head)(pth).flatMap { r0 =>
      h.value.matchPath(r0.rest).map { r =>
        PathMatchResult((r0.value._1, r0.value._2, r0.value._3, r0.value._4, r0.value._5, r0.value._6, r0.value._7, r.value), r.rest)
      }
    }
  }

  def matcher9[T1, T2, T3, T4, T5, T6, T7, T8, T9](h: HElem9[T1, T2, T3, T4, T5, T6, T7, T8, T9]): Path => Option[PathMatchResult[(T1, T2, T3, T4, T5, T6, T7, T8, T9), Path]] = { pth =>
    matcher8(h.head)(pth).flatMap { r0 =>
      h.value.matchPath(r0.rest).map { r =>
        PathMatchResult((r0.value._1, r0.value._2, r0.value._3, r0.value._4, r0.value._5, r0.value._6, r0.value._7, r0.value._8, r.value), r.rest)
      }
    }
  }

  def matcher10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10](h: HElem10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10]): Path => Option[PathMatchResult[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10), Path]] = { pth =>
    matcher9(h.head)(pth).flatMap { r0 =>
      h.value.matchPath(r0.rest).map { r =>
        PathMatchResult((r0.value._1, r0.value._2, r0.value._3, r0.value._4, r0.value._5, r0.value._6, r0.value._7, r0.value._8, r0.value._9, r.value), r.rest)
      }
    }
  }

  def matcher11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11](h: HElem11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11]): Path => Option[PathMatchResult[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11), Path]] = { pth =>
    matcher10(h.head)(pth).flatMap { r0 =>
      h.value.matchPath(r0.rest).map { r =>
        PathMatchResult((r0.value._1, r0.value._2, r0.value._3, r0.value._4, r0.value._5, r0.value._6, r0.value._7, r0.value._8, r0.value._9, r0.value._10, r.value), r.rest)
      }
    }
  }

  def matcher12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12](h: HElem12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12]): Path => Option[PathMatchResult[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12), Path]] = { pth =>
    matcher11(h.head)(pth).flatMap { r0 =>
      h.value.matchPath(r0.rest).map { r =>
        PathMatchResult((r0.value._1, r0.value._2, r0.value._3, r0.value._4, r0.value._5, r0.value._6, r0.value._7, r0.value._8, r0.value._9, r0.value._10, r0.value._11, r.value), r.rest)
      }
    }
  }

  def matcher13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13](h: HElem13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13]): Path => Option[PathMatchResult[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13), Path]] = { pth =>
    matcher12(h.head)(pth).flatMap { r0 =>
      h.value.matchPath(r0.rest).map { r =>
        PathMatchResult((r0.value._1, r0.value._2, r0.value._3, r0.value._4, r0.value._5, r0.value._6, r0.value._7, r0.value._8, r0.value._9, r0.value._10, r0.value._11, r0.value._12, r.value), r.rest)
      }
    }
  }

  def matcher14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14](h: HElem14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14]): Path => Option[PathMatchResult[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14), Path]] = { pth =>
    matcher13(h.head)(pth).flatMap { r0 =>
      h.value.matchPath(r0.rest).map { r =>
        PathMatchResult((r0.value._1, r0.value._2, r0.value._3, r0.value._4, r0.value._5, r0.value._6, r0.value._7, r0.value._8, r0.value._9, r0.value._10, r0.value._11, r0.value._12, r0.value._13, r.value), r.rest)
      }
    }
  }

  def matcher15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15](h: HElem15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15]): Path => Option[PathMatchResult[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15), Path]] = { pth =>
    matcher14(h.head)(pth).flatMap { r0 =>
      h.value.matchPath(r0.rest).map { r =>
        PathMatchResult((r0.value._1, r0.value._2, r0.value._3, r0.value._4, r0.value._5, r0.value._6, r0.value._7, r0.value._8, r0.value._9, r0.value._10, r0.value._11, r0.value._12, r0.value._13, r0.value._14, r.value), r.rest)
      }
    }
  }

  def matcher16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16](h: HElem16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16]): Path => Option[PathMatchResult[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16), Path]] = { pth =>
    matcher15(h.head)(pth).flatMap { r0 =>
      h.value.matchPath(r0.rest).map { r =>
        PathMatchResult((r0.value._1, r0.value._2, r0.value._3, r0.value._4, r0.value._5, r0.value._6, r0.value._7, r0.value._8, r0.value._9, r0.value._10, r0.value._11, r0.value._12, r0.value._13, r0.value._14, r0.value._15, r.value), r.rest)
      }
    }
  }

  def matcher17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17](h: HElem17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17]): Path => Option[PathMatchResult[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17), Path]] = { pth =>
    matcher16(h.head)(pth).flatMap { r0 =>
      h.value.matchPath(r0.rest).map { r =>
        PathMatchResult((r0.value._1, r0.value._2, r0.value._3, r0.value._4, r0.value._5, r0.value._6, r0.value._7, r0.value._8, r0.value._9, r0.value._10, r0.value._11, r0.value._12, r0.value._13, r0.value._14, r0.value._15, r0.value._16, r.value), r.rest)
      }
    }
  }

  def matcher18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18](h: HElem18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18]): Path => Option[PathMatchResult[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18), Path]] = { pth =>
    matcher17(h.head)(pth).flatMap { r0 =>
      h.value.matchPath(r0.rest).map { r =>
        PathMatchResult((r0.value._1, r0.value._2, r0.value._3, r0.value._4, r0.value._5, r0.value._6, r0.value._7, r0.value._8, r0.value._9, r0.value._10, r0.value._11, r0.value._12, r0.value._13, r0.value._14, r0.value._15, r0.value._16, r0.value._17, r.value), r.rest)
      }
    }
  }

  def matcher19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19](h: HElem19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]): Path => Option[PathMatchResult[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19), Path]] = { pth =>
    matcher18(h.head)(pth).flatMap { r0 =>
      h.value.matchPath(r0.rest).map { r =>
        PathMatchResult((r0.value._1, r0.value._2, r0.value._3, r0.value._4, r0.value._5, r0.value._6, r0.value._7, r0.value._8, r0.value._9, r0.value._10, r0.value._11, r0.value._12, r0.value._13, r0.value._14, r0.value._15, r0.value._16, r0.value._17, r0.value._18, r.value), r.rest)
      }
    }
  }

  def matcher20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20](h: HElem20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20]): Path => Option[PathMatchResult[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20), Path]] = { pth =>
    matcher19(h.head)(pth).flatMap { r0 =>
      h.value.matchPath(r0.rest).map { r =>
        PathMatchResult((r0.value._1, r0.value._2, r0.value._3, r0.value._4, r0.value._5, r0.value._6, r0.value._7, r0.value._8, r0.value._9, r0.value._10, r0.value._11, r0.value._12, r0.value._13, r0.value._14, r0.value._15, r0.value._16, r0.value._17, r0.value._18, r0.value._19, r.value), r.rest)
      }
    }
  }

  def matcher21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21](h: HElem21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21]): Path => Option[PathMatchResult[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21), Path]] = { pth =>
    matcher20(h.head)(pth).flatMap { r0 =>
      h.value.matchPath(r0.rest).map { r =>
        PathMatchResult((r0.value._1, r0.value._2, r0.value._3, r0.value._4, r0.value._5, r0.value._6, r0.value._7, r0.value._8, r0.value._9, r0.value._10, r0.value._11, r0.value._12, r0.value._13, r0.value._14, r0.value._15, r0.value._16, r0.value._17, r0.value._18, r0.value._19, r0.value._20, r.value), r.rest)
      }
    }
  }

  def matcher22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22](h: HElem22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22]): Path => Option[PathMatchResult[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22), Path]] = { pth =>
    matcher21(h.head)(pth).flatMap { r0 =>
      h.value.matchPath(r0.rest).map { r =>
        PathMatchResult((r0.value._1, r0.value._2, r0.value._3, r0.value._4, r0.value._5, r0.value._6, r0.value._7, r0.value._8, r0.value._9, r0.value._10, r0.value._11, r0.value._12, r0.value._13, r0.value._14, r0.value._15, r0.value._16, r0.value._17, r0.value._18, r0.value._19, r0.value._20, r0.value._21, r.value), r.rest)
      }
    }
  }

}

object BasePathMatchers extends BasePathMatchers 