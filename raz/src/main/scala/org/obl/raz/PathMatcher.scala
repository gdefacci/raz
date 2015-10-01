package org.obl.raz

import scalaz.{ -\/, \/, \/- }

trait PathMatcher[-H, T] {
  def decoder(h: H): PathDecoder[T]
}

object PathMatcher {

  import DecHPaths._

  def apply[H, T](f: H => PathDecoder[T]) =
    new PathMatcher[H, T] {
      def decoder(h: H) = f(h)
    }

  //  implicit def pathMatcher[P <: PathPosition, S <: P] = apply[BasePath[P,S], Path] { path =>
  //    PathDecoder( (p:Path) => PathUtils.subtract(p,path).map( rest => PathMatchResult(path, rest) ) )
  //  }

  implicit def pathMatcher = apply[Path, Path] { path =>
    PathDecoder((p: Path) => PathUtils.subtract(p, path).map(rest => PathMatchResult(path, rest)))
  }

  implicit def matcher1[T1] = apply[DecHPath1[T1], T1]({ h =>
    PathDecoder[T1] { pth =>
      for (
        r0 <- PathUtils.subtract(pth, h.head.path);
        r1 <- h.value.decode(r0)
      ) yield r1
      //      PathUtils.subtract(pth, h.head.path).flatMap(r0 => h.value.decode(r0))
    }
  })

  implicit def matcher2[T1, T2] = apply[DecHPath2[T1, T2], (T1, T2)]({ h =>
    lazy val d1 = matcher1[T1].decoder(h.head)
    lazy val d2 = h.value
    PathDecoder { pth =>
      for (
        r1 <- d1.decode(pth);
        r2 <- d2.decode(r1.rest)
      ) yield {
        r2.mapValue((v: T2) => (r1.value, v))
      }
    }
  })

  implicit def matcher3[T1, T2, T3] = apply[DecHPath3[T1, T2, T3], (T1, T2, T3)]({ h =>
    lazy val d1 = matcher1[T1].decoder(h.head.head)
    lazy val d2 = h.head.value
    lazy val d3 = h.value
    PathDecoder { pth =>
      for (
        r1 <- d1.decode(pth);
        r2 <- d2.decode(r1.rest);
        r3 <- d3.decode(r2.rest)
      ) yield {
        r3.mapValue((v: T3) => (r1.value, r2.value, v))
      }
    }
  })

  implicit def matcher4[T1, T2, T3, T4] = apply[DecHPath4[T1, T2, T3, T4], (T1, T2, T3, T4)]({ h =>
    lazy val d1 = matcher1[T1].decoder(h.head.head.head)
    lazy val d2 = h.head.head.value
    lazy val d3 = h.head.value
    lazy val d4 = h.value
    PathDecoder { pth =>
      for (
        r1 <- d1.decode(pth);
        r2 <- d2.decode(r1.rest);
        r3 <- d3.decode(r2.rest);
        r4 <- d4.decode(r3.rest)
      ) yield {
        r4.mapValue((v: T4) => (r1.value, r2.value, r3.value, v))
      }
    }
  })

  implicit def matcher5[T1, T2, T3, T4, T5] = apply[DecHPath5[T1, T2, T3, T4, T5], (T1, T2, T3, T4, T5)]({ h =>
    lazy val d1 = matcher1[T1].decoder(h.head.head.head.head)
    lazy val d2 = h.head.head.head.value
    lazy val d3 = h.head.head.value
    lazy val d4 = h.head.value
    lazy val d5 = h.value
    PathDecoder { pth =>
      for (
        r1 <- d1.decode(pth);
        r2 <- d2.decode(r1.rest);
        r3 <- d3.decode(r2.rest);
        r4 <- d4.decode(r3.rest);
        r5 <- d5.decode(r4.rest)
      ) yield {
        r5.mapValue((v: T5) => (r1.value, r2.value, r3.value, r4.value, v))
      }
    }
  })

  implicit def matcher6[T1, T2, T3, T4, T5, T6] = apply[DecHPath6[T1, T2, T3, T4, T5, T6], (T1, T2, T3, T4, T5, T6)]({ h =>
    lazy val d1 = matcher1[T1].decoder(h.head.head.head.head.head)
    lazy val d2 = h.head.head.head.head.value
    lazy val d3 = h.head.head.head.value
    lazy val d4 = h.head.head.value
    lazy val d5 = h.head.value
    lazy val d6 = h.value
    PathDecoder { pth =>
      for (
        r1 <- d1.decode(pth);
        r2 <- d2.decode(r1.rest);
        r3 <- d3.decode(r2.rest);
        r4 <- d4.decode(r3.rest);
        r5 <- d5.decode(r4.rest);
        r6 <- d6.decode(r5.rest)
      ) yield {
        r6.mapValue((v: T6) => (r1.value, r2.value, r3.value, r4.value, r5.value, v))
      }
    }
  })

  implicit def matcher7[T1, T2, T3, T4, T5, T6, T7] = apply[DecHPath7[T1, T2, T3, T4, T5, T6, T7], (T1, T2, T3, T4, T5, T6, T7)]({ h =>
    lazy val d1 = matcher1[T1].decoder(h.head.head.head.head.head.head)
    lazy val d2 = h.head.head.head.head.head.value
    lazy val d3 = h.head.head.head.head.value
    lazy val d4 = h.head.head.head.value
    lazy val d5 = h.head.head.value
    lazy val d6 = h.head.value
    lazy val d7 = h.value
    PathDecoder { pth =>
      for (
        r1 <- d1.decode(pth);
        r2 <- d2.decode(r1.rest);
        r3 <- d3.decode(r2.rest);
        r4 <- d4.decode(r3.rest);
        r5 <- d5.decode(r4.rest);
        r6 <- d6.decode(r5.rest);
        r7 <- d7.decode(r6.rest)
      ) yield {
        r7.mapValue((v: T7) => (r1.value, r2.value, r3.value, r4.value, r5.value, r6.value, v))
      }
    }
  })

  implicit def matcher8[T1, T2, T3, T4, T5, T6, T7, T8] = apply[DecHPath8[T1, T2, T3, T4, T5, T6, T7, T8], (T1, T2, T3, T4, T5, T6, T7, T8)]({ h =>
    lazy val d1 = matcher1[T1].decoder(h.head.head.head.head.head.head.head)
    lazy val d2 = h.head.head.head.head.head.head.value
    lazy val d3 = h.head.head.head.head.head.value
    lazy val d4 = h.head.head.head.head.value
    lazy val d5 = h.head.head.head.value
    lazy val d6 = h.head.head.value
    lazy val d7 = h.head.value
    lazy val d8 = h.value
    PathDecoder { pth =>
      for (
        r1 <- d1.decode(pth);
        r2 <- d2.decode(r1.rest);
        r3 <- d3.decode(r2.rest);
        r4 <- d4.decode(r3.rest);
        r5 <- d5.decode(r4.rest);
        r6 <- d6.decode(r5.rest);
        r7 <- d7.decode(r6.rest);
        r8 <- d8.decode(r7.rest)
      ) yield {
        r8.mapValue((v: T8) => (r1.value, r2.value, r3.value, r4.value, r5.value, r6.value, r7.value, v))
      }
    }
  })

  implicit def matcher9[T1, T2, T3, T4, T5, T6, T7, T8, T9] = apply[DecHPath9[T1, T2, T3, T4, T5, T6, T7, T8, T9], (T1, T2, T3, T4, T5, T6, T7, T8, T9)]({ h =>
    lazy val d1 = matcher1[T1].decoder(h.head.head.head.head.head.head.head.head)
    lazy val d2 = h.head.head.head.head.head.head.head.value
    lazy val d3 = h.head.head.head.head.head.head.value
    lazy val d4 = h.head.head.head.head.head.value
    lazy val d5 = h.head.head.head.head.value
    lazy val d6 = h.head.head.head.value
    lazy val d7 = h.head.head.value
    lazy val d8 = h.head.value
    lazy val d9 = h.value
    PathDecoder { pth =>
      for (
        r1 <- d1.decode(pth);
        r2 <- d2.decode(r1.rest);
        r3 <- d3.decode(r2.rest);
        r4 <- d4.decode(r3.rest);
        r5 <- d5.decode(r4.rest);
        r6 <- d6.decode(r5.rest);
        r7 <- d7.decode(r6.rest);
        r8 <- d8.decode(r7.rest);
        r9 <- d9.decode(r8.rest)
      ) yield {
        r9.mapValue((v: T9) => (r1.value, r2.value, r3.value, r4.value, r5.value, r6.value, r7.value, r8.value, v))
      }
    }
  })

  implicit def matcher10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10] = apply[DecHPath10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10], (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10)]({ h =>
    lazy val d1 = matcher1[T1].decoder(h.head.head.head.head.head.head.head.head.head)
    lazy val d2 = h.head.head.head.head.head.head.head.head.value
    lazy val d3 = h.head.head.head.head.head.head.head.value
    lazy val d4 = h.head.head.head.head.head.head.value
    lazy val d5 = h.head.head.head.head.head.value
    lazy val d6 = h.head.head.head.head.value
    lazy val d7 = h.head.head.head.value
    lazy val d8 = h.head.head.value
    lazy val d9 = h.head.value
    lazy val d10 = h.value
    PathDecoder { pth =>
      for (
        r1 <- d1.decode(pth);
        r2 <- d2.decode(r1.rest);
        r3 <- d3.decode(r2.rest);
        r4 <- d4.decode(r3.rest);
        r5 <- d5.decode(r4.rest);
        r6 <- d6.decode(r5.rest);
        r7 <- d7.decode(r6.rest);
        r8 <- d8.decode(r7.rest);
        r9 <- d9.decode(r8.rest);
        r10 <- d10.decode(r9.rest)
      ) yield {
        r10.mapValue((v: T10) => (r1.value, r2.value, r3.value, r4.value, r5.value, r6.value, r7.value, r8.value, r9.value, v))
      }
    }
  })

  implicit def matcher11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11] = apply[DecHPath11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11], (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11)]({ h =>
    lazy val d1 = matcher1[T1].decoder(h.head.head.head.head.head.head.head.head.head.head)
    lazy val d2 = h.head.head.head.head.head.head.head.head.head.value
    lazy val d3 = h.head.head.head.head.head.head.head.head.value
    lazy val d4 = h.head.head.head.head.head.head.head.value
    lazy val d5 = h.head.head.head.head.head.head.value
    lazy val d6 = h.head.head.head.head.head.value
    lazy val d7 = h.head.head.head.head.value
    lazy val d8 = h.head.head.head.value
    lazy val d9 = h.head.head.value
    lazy val d10 = h.head.value
    lazy val d11 = h.value
    PathDecoder { pth =>
      for (
        r1 <- d1.decode(pth);
        r2 <- d2.decode(r1.rest);
        r3 <- d3.decode(r2.rest);
        r4 <- d4.decode(r3.rest);
        r5 <- d5.decode(r4.rest);
        r6 <- d6.decode(r5.rest);
        r7 <- d7.decode(r6.rest);
        r8 <- d8.decode(r7.rest);
        r9 <- d9.decode(r8.rest);
        r10 <- d10.decode(r9.rest);
        r11 <- d11.decode(r10.rest)
      ) yield {
        r11.mapValue((v: T11) => (r1.value, r2.value, r3.value, r4.value, r5.value, r6.value, r7.value, r8.value, r9.value, r10.value, v))
      }
    }
  })

  implicit def matcher12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12] = apply[DecHPath12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12], (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12)]({ h =>
    lazy val d1 = matcher1[T1].decoder(h.head.head.head.head.head.head.head.head.head.head.head)
    lazy val d2 = h.head.head.head.head.head.head.head.head.head.head.value
    lazy val d3 = h.head.head.head.head.head.head.head.head.head.value
    lazy val d4 = h.head.head.head.head.head.head.head.head.value
    lazy val d5 = h.head.head.head.head.head.head.head.value
    lazy val d6 = h.head.head.head.head.head.head.value
    lazy val d7 = h.head.head.head.head.head.value
    lazy val d8 = h.head.head.head.head.value
    lazy val d9 = h.head.head.head.value
    lazy val d10 = h.head.head.value
    lazy val d11 = h.head.value
    lazy val d12 = h.value
    PathDecoder { pth =>
      for (
        r1 <- d1.decode(pth);
        r2 <- d2.decode(r1.rest);
        r3 <- d3.decode(r2.rest);
        r4 <- d4.decode(r3.rest);
        r5 <- d5.decode(r4.rest);
        r6 <- d6.decode(r5.rest);
        r7 <- d7.decode(r6.rest);
        r8 <- d8.decode(r7.rest);
        r9 <- d9.decode(r8.rest);
        r10 <- d10.decode(r9.rest);
        r11 <- d11.decode(r10.rest);
        r12 <- d12.decode(r11.rest)
      ) yield {
        r12.mapValue((v: T12) => (r1.value, r2.value, r3.value, r4.value, r5.value, r6.value, r7.value, r8.value, r9.value, r10.value, r11.value, v))
      }
    }
  })

  implicit def matcher13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13] = apply[DecHPath13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13], (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13)]({ h =>
    lazy val d1 = matcher1[T1].decoder(h.head.head.head.head.head.head.head.head.head.head.head.head)
    lazy val d2 = h.head.head.head.head.head.head.head.head.head.head.head.value
    lazy val d3 = h.head.head.head.head.head.head.head.head.head.head.value
    lazy val d4 = h.head.head.head.head.head.head.head.head.head.value
    lazy val d5 = h.head.head.head.head.head.head.head.head.value
    lazy val d6 = h.head.head.head.head.head.head.head.value
    lazy val d7 = h.head.head.head.head.head.head.value
    lazy val d8 = h.head.head.head.head.head.value
    lazy val d9 = h.head.head.head.head.value
    lazy val d10 = h.head.head.head.value
    lazy val d11 = h.head.head.value
    lazy val d12 = h.head.value
    lazy val d13 = h.value
    PathDecoder { pth =>
      for (
        r1 <- d1.decode(pth);
        r2 <- d2.decode(r1.rest);
        r3 <- d3.decode(r2.rest);
        r4 <- d4.decode(r3.rest);
        r5 <- d5.decode(r4.rest);
        r6 <- d6.decode(r5.rest);
        r7 <- d7.decode(r6.rest);
        r8 <- d8.decode(r7.rest);
        r9 <- d9.decode(r8.rest);
        r10 <- d10.decode(r9.rest);
        r11 <- d11.decode(r10.rest);
        r12 <- d12.decode(r11.rest);
        r13 <- d13.decode(r12.rest)
      ) yield {
        r13.mapValue((v: T13) => (r1.value, r2.value, r3.value, r4.value, r5.value, r6.value, r7.value, r8.value, r9.value, r10.value, r11.value, r12.value, v))
      }
    }
  })

  implicit def matcher14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14] = apply[DecHPath14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14], (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14)]({ h =>
    lazy val d1 = matcher1[T1].decoder(h.head.head.head.head.head.head.head.head.head.head.head.head.head)
    lazy val d2 = h.head.head.head.head.head.head.head.head.head.head.head.head.value
    lazy val d3 = h.head.head.head.head.head.head.head.head.head.head.head.value
    lazy val d4 = h.head.head.head.head.head.head.head.head.head.head.value
    lazy val d5 = h.head.head.head.head.head.head.head.head.head.value
    lazy val d6 = h.head.head.head.head.head.head.head.head.value
    lazy val d7 = h.head.head.head.head.head.head.head.value
    lazy val d8 = h.head.head.head.head.head.head.value
    lazy val d9 = h.head.head.head.head.head.value
    lazy val d10 = h.head.head.head.head.value
    lazy val d11 = h.head.head.head.value
    lazy val d12 = h.head.head.value
    lazy val d13 = h.head.value
    lazy val d14 = h.value
    PathDecoder { pth =>
      for (
        r1 <- d1.decode(pth);
        r2 <- d2.decode(r1.rest);
        r3 <- d3.decode(r2.rest);
        r4 <- d4.decode(r3.rest);
        r5 <- d5.decode(r4.rest);
        r6 <- d6.decode(r5.rest);
        r7 <- d7.decode(r6.rest);
        r8 <- d8.decode(r7.rest);
        r9 <- d9.decode(r8.rest);
        r10 <- d10.decode(r9.rest);
        r11 <- d11.decode(r10.rest);
        r12 <- d12.decode(r11.rest);
        r13 <- d13.decode(r12.rest);
        r14 <- d14.decode(r13.rest)
      ) yield {
        r14.mapValue((v: T14) => (r1.value, r2.value, r3.value, r4.value, r5.value, r6.value, r7.value, r8.value, r9.value, r10.value, r11.value, r12.value, r13.value, v))
      }
    }
  })

  implicit def matcher15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15] = apply[DecHPath15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15], (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15)]({ h =>
    lazy val d1 = matcher1[T1].decoder(h.head.head.head.head.head.head.head.head.head.head.head.head.head.head)
    lazy val d2 = h.head.head.head.head.head.head.head.head.head.head.head.head.head.value
    lazy val d3 = h.head.head.head.head.head.head.head.head.head.head.head.head.value
    lazy val d4 = h.head.head.head.head.head.head.head.head.head.head.head.value
    lazy val d5 = h.head.head.head.head.head.head.head.head.head.head.value
    lazy val d6 = h.head.head.head.head.head.head.head.head.head.value
    lazy val d7 = h.head.head.head.head.head.head.head.head.value
    lazy val d8 = h.head.head.head.head.head.head.head.value
    lazy val d9 = h.head.head.head.head.head.head.value
    lazy val d10 = h.head.head.head.head.head.value
    lazy val d11 = h.head.head.head.head.value
    lazy val d12 = h.head.head.head.value
    lazy val d13 = h.head.head.value
    lazy val d14 = h.head.value
    lazy val d15 = h.value
    PathDecoder { pth =>
      for (
        r1 <- d1.decode(pth);
        r2 <- d2.decode(r1.rest);
        r3 <- d3.decode(r2.rest);
        r4 <- d4.decode(r3.rest);
        r5 <- d5.decode(r4.rest);
        r6 <- d6.decode(r5.rest);
        r7 <- d7.decode(r6.rest);
        r8 <- d8.decode(r7.rest);
        r9 <- d9.decode(r8.rest);
        r10 <- d10.decode(r9.rest);
        r11 <- d11.decode(r10.rest);
        r12 <- d12.decode(r11.rest);
        r13 <- d13.decode(r12.rest);
        r14 <- d14.decode(r13.rest);
        r15 <- d15.decode(r14.rest)
      ) yield {
        r15.mapValue((v: T15) => (r1.value, r2.value, r3.value, r4.value, r5.value, r6.value, r7.value, r8.value, r9.value, r10.value, r11.value, r12.value, r13.value, r14.value, v))
      }
    }
  })

  implicit def matcher16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16] = apply[DecHPath16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16], (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16)]({ h =>
    lazy val d1 = matcher1[T1].decoder(h.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head)
    lazy val d2 = h.head.head.head.head.head.head.head.head.head.head.head.head.head.head.value
    lazy val d3 = h.head.head.head.head.head.head.head.head.head.head.head.head.head.value
    lazy val d4 = h.head.head.head.head.head.head.head.head.head.head.head.head.value
    lazy val d5 = h.head.head.head.head.head.head.head.head.head.head.head.value
    lazy val d6 = h.head.head.head.head.head.head.head.head.head.head.value
    lazy val d7 = h.head.head.head.head.head.head.head.head.head.value
    lazy val d8 = h.head.head.head.head.head.head.head.head.value
    lazy val d9 = h.head.head.head.head.head.head.head.value
    lazy val d10 = h.head.head.head.head.head.head.value
    lazy val d11 = h.head.head.head.head.head.value
    lazy val d12 = h.head.head.head.head.value
    lazy val d13 = h.head.head.head.value
    lazy val d14 = h.head.head.value
    lazy val d15 = h.head.value
    lazy val d16 = h.value
    PathDecoder { pth =>
      for (
        r1 <- d1.decode(pth);
        r2 <- d2.decode(r1.rest);
        r3 <- d3.decode(r2.rest);
        r4 <- d4.decode(r3.rest);
        r5 <- d5.decode(r4.rest);
        r6 <- d6.decode(r5.rest);
        r7 <- d7.decode(r6.rest);
        r8 <- d8.decode(r7.rest);
        r9 <- d9.decode(r8.rest);
        r10 <- d10.decode(r9.rest);
        r11 <- d11.decode(r10.rest);
        r12 <- d12.decode(r11.rest);
        r13 <- d13.decode(r12.rest);
        r14 <- d14.decode(r13.rest);
        r15 <- d15.decode(r14.rest);
        r16 <- d16.decode(r15.rest)
      ) yield {
        r16.mapValue((v: T16) => (r1.value, r2.value, r3.value, r4.value, r5.value, r6.value, r7.value, r8.value, r9.value, r10.value, r11.value, r12.value, r13.value, r14.value, r15.value, v))
      }
    }
  })

  implicit def matcher17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17] = apply[DecHPath17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17], (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17)]({ h =>
    lazy val d1 = matcher1[T1].decoder(h.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head)
    lazy val d2 = h.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.value
    lazy val d3 = h.head.head.head.head.head.head.head.head.head.head.head.head.head.head.value
    lazy val d4 = h.head.head.head.head.head.head.head.head.head.head.head.head.head.value
    lazy val d5 = h.head.head.head.head.head.head.head.head.head.head.head.head.value
    lazy val d6 = h.head.head.head.head.head.head.head.head.head.head.head.value
    lazy val d7 = h.head.head.head.head.head.head.head.head.head.head.value
    lazy val d8 = h.head.head.head.head.head.head.head.head.head.value
    lazy val d9 = h.head.head.head.head.head.head.head.head.value
    lazy val d10 = h.head.head.head.head.head.head.head.value
    lazy val d11 = h.head.head.head.head.head.head.value
    lazy val d12 = h.head.head.head.head.head.value
    lazy val d13 = h.head.head.head.head.value
    lazy val d14 = h.head.head.head.value
    lazy val d15 = h.head.head.value
    lazy val d16 = h.head.value
    lazy val d17 = h.value
    PathDecoder { pth =>
      for (
        r1 <- d1.decode(pth);
        r2 <- d2.decode(r1.rest);
        r3 <- d3.decode(r2.rest);
        r4 <- d4.decode(r3.rest);
        r5 <- d5.decode(r4.rest);
        r6 <- d6.decode(r5.rest);
        r7 <- d7.decode(r6.rest);
        r8 <- d8.decode(r7.rest);
        r9 <- d9.decode(r8.rest);
        r10 <- d10.decode(r9.rest);
        r11 <- d11.decode(r10.rest);
        r12 <- d12.decode(r11.rest);
        r13 <- d13.decode(r12.rest);
        r14 <- d14.decode(r13.rest);
        r15 <- d15.decode(r14.rest);
        r16 <- d16.decode(r15.rest);
        r17 <- d17.decode(r16.rest)
      ) yield {
        r17.mapValue((v: T17) => (r1.value, r2.value, r3.value, r4.value, r5.value, r6.value, r7.value, r8.value, r9.value, r10.value, r11.value, r12.value, r13.value, r14.value, r15.value, r16.value, v))
      }
    }
  })

  implicit def matcher18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18] = apply[DecHPath18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18], (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18)]({ h =>
    lazy val d1 = matcher1[T1].decoder(h.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head)
    lazy val d2 = h.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.value
    lazy val d3 = h.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.value
    lazy val d4 = h.head.head.head.head.head.head.head.head.head.head.head.head.head.head.value
    lazy val d5 = h.head.head.head.head.head.head.head.head.head.head.head.head.head.value
    lazy val d6 = h.head.head.head.head.head.head.head.head.head.head.head.head.value
    lazy val d7 = h.head.head.head.head.head.head.head.head.head.head.head.value
    lazy val d8 = h.head.head.head.head.head.head.head.head.head.head.value
    lazy val d9 = h.head.head.head.head.head.head.head.head.head.value
    lazy val d10 = h.head.head.head.head.head.head.head.head.value
    lazy val d11 = h.head.head.head.head.head.head.head.value
    lazy val d12 = h.head.head.head.head.head.head.value
    lazy val d13 = h.head.head.head.head.head.value
    lazy val d14 = h.head.head.head.head.value
    lazy val d15 = h.head.head.head.value
    lazy val d16 = h.head.head.value
    lazy val d17 = h.head.value
    lazy val d18 = h.value
    PathDecoder { pth =>
      for (
        r1 <- d1.decode(pth);
        r2 <- d2.decode(r1.rest);
        r3 <- d3.decode(r2.rest);
        r4 <- d4.decode(r3.rest);
        r5 <- d5.decode(r4.rest);
        r6 <- d6.decode(r5.rest);
        r7 <- d7.decode(r6.rest);
        r8 <- d8.decode(r7.rest);
        r9 <- d9.decode(r8.rest);
        r10 <- d10.decode(r9.rest);
        r11 <- d11.decode(r10.rest);
        r12 <- d12.decode(r11.rest);
        r13 <- d13.decode(r12.rest);
        r14 <- d14.decode(r13.rest);
        r15 <- d15.decode(r14.rest);
        r16 <- d16.decode(r15.rest);
        r17 <- d17.decode(r16.rest);
        r18 <- d18.decode(r17.rest)
      ) yield {
        r18.mapValue((v: T18) => (r1.value, r2.value, r3.value, r4.value, r5.value, r6.value, r7.value, r8.value, r9.value, r10.value, r11.value, r12.value, r13.value, r14.value, r15.value, r16.value, r17.value, v))
      }
    }
  })

  implicit def matcher19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19] = apply[DecHPath19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19], (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19)]({ h =>
    lazy val d1 = matcher1[T1].decoder(h.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head)
    lazy val d2 = h.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.value
    lazy val d3 = h.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.value
    lazy val d4 = h.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.value
    lazy val d5 = h.head.head.head.head.head.head.head.head.head.head.head.head.head.head.value
    lazy val d6 = h.head.head.head.head.head.head.head.head.head.head.head.head.head.value
    lazy val d7 = h.head.head.head.head.head.head.head.head.head.head.head.head.value
    lazy val d8 = h.head.head.head.head.head.head.head.head.head.head.head.value
    lazy val d9 = h.head.head.head.head.head.head.head.head.head.head.value
    lazy val d10 = h.head.head.head.head.head.head.head.head.head.value
    lazy val d11 = h.head.head.head.head.head.head.head.head.value
    lazy val d12 = h.head.head.head.head.head.head.head.value
    lazy val d13 = h.head.head.head.head.head.head.value
    lazy val d14 = h.head.head.head.head.head.value
    lazy val d15 = h.head.head.head.head.value
    lazy val d16 = h.head.head.head.value
    lazy val d17 = h.head.head.value
    lazy val d18 = h.head.value
    lazy val d19 = h.value
    PathDecoder { pth =>
      for (
        r1 <- d1.decode(pth);
        r2 <- d2.decode(r1.rest);
        r3 <- d3.decode(r2.rest);
        r4 <- d4.decode(r3.rest);
        r5 <- d5.decode(r4.rest);
        r6 <- d6.decode(r5.rest);
        r7 <- d7.decode(r6.rest);
        r8 <- d8.decode(r7.rest);
        r9 <- d9.decode(r8.rest);
        r10 <- d10.decode(r9.rest);
        r11 <- d11.decode(r10.rest);
        r12 <- d12.decode(r11.rest);
        r13 <- d13.decode(r12.rest);
        r14 <- d14.decode(r13.rest);
        r15 <- d15.decode(r14.rest);
        r16 <- d16.decode(r15.rest);
        r17 <- d17.decode(r16.rest);
        r18 <- d18.decode(r17.rest);
        r19 <- d19.decode(r18.rest)
      ) yield {
        r19.mapValue((v: T19) => (r1.value, r2.value, r3.value, r4.value, r5.value, r6.value, r7.value, r8.value, r9.value, r10.value, r11.value, r12.value, r13.value, r14.value, r15.value, r16.value, r17.value, r18.value, v))
      }
    }
  })

  implicit def matcher20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20] = apply[DecHPath20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20], (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20)]({ h =>
    lazy val d1 = matcher1[T1].decoder(h.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head)
    lazy val d2 = h.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.value
    lazy val d3 = h.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.value
    lazy val d4 = h.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.value
    lazy val d5 = h.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.value
    lazy val d6 = h.head.head.head.head.head.head.head.head.head.head.head.head.head.head.value
    lazy val d7 = h.head.head.head.head.head.head.head.head.head.head.head.head.head.value
    lazy val d8 = h.head.head.head.head.head.head.head.head.head.head.head.head.value
    lazy val d9 = h.head.head.head.head.head.head.head.head.head.head.head.value
    lazy val d10 = h.head.head.head.head.head.head.head.head.head.head.value
    lazy val d11 = h.head.head.head.head.head.head.head.head.head.value
    lazy val d12 = h.head.head.head.head.head.head.head.head.value
    lazy val d13 = h.head.head.head.head.head.head.head.value
    lazy val d14 = h.head.head.head.head.head.head.value
    lazy val d15 = h.head.head.head.head.head.value
    lazy val d16 = h.head.head.head.head.value
    lazy val d17 = h.head.head.head.value
    lazy val d18 = h.head.head.value
    lazy val d19 = h.head.value
    lazy val d20 = h.value
    PathDecoder { pth =>
      for (
        r1 <- d1.decode(pth);
        r2 <- d2.decode(r1.rest);
        r3 <- d3.decode(r2.rest);
        r4 <- d4.decode(r3.rest);
        r5 <- d5.decode(r4.rest);
        r6 <- d6.decode(r5.rest);
        r7 <- d7.decode(r6.rest);
        r8 <- d8.decode(r7.rest);
        r9 <- d9.decode(r8.rest);
        r10 <- d10.decode(r9.rest);
        r11 <- d11.decode(r10.rest);
        r12 <- d12.decode(r11.rest);
        r13 <- d13.decode(r12.rest);
        r14 <- d14.decode(r13.rest);
        r15 <- d15.decode(r14.rest);
        r16 <- d16.decode(r15.rest);
        r17 <- d17.decode(r16.rest);
        r18 <- d18.decode(r17.rest);
        r19 <- d19.decode(r18.rest);
        r20 <- d20.decode(r19.rest)
      ) yield {
        r20.mapValue((v: T20) => (r1.value, r2.value, r3.value, r4.value, r5.value, r6.value, r7.value, r8.value, r9.value, r10.value, r11.value, r12.value, r13.value, r14.value, r15.value, r16.value, r17.value, r18.value, r19.value, v))
      }
    }
  })

  implicit def matcher21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21] = apply[DecHPath21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21], (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21)]({ h =>
    lazy val d1 = matcher1[T1].decoder(h.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head)
    lazy val d2 = h.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.value
    lazy val d3 = h.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.value
    lazy val d4 = h.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.value
    lazy val d5 = h.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.value
    lazy val d6 = h.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.value
    lazy val d7 = h.head.head.head.head.head.head.head.head.head.head.head.head.head.head.value
    lazy val d8 = h.head.head.head.head.head.head.head.head.head.head.head.head.head.value
    lazy val d9 = h.head.head.head.head.head.head.head.head.head.head.head.head.value
    lazy val d10 = h.head.head.head.head.head.head.head.head.head.head.head.value
    lazy val d11 = h.head.head.head.head.head.head.head.head.head.head.value
    lazy val d12 = h.head.head.head.head.head.head.head.head.head.value
    lazy val d13 = h.head.head.head.head.head.head.head.head.value
    lazy val d14 = h.head.head.head.head.head.head.head.value
    lazy val d15 = h.head.head.head.head.head.head.value
    lazy val d16 = h.head.head.head.head.head.value
    lazy val d17 = h.head.head.head.head.value
    lazy val d18 = h.head.head.head.value
    lazy val d19 = h.head.head.value
    lazy val d20 = h.head.value
    lazy val d21 = h.value
    PathDecoder { pth =>
      for (
        r1 <- d1.decode(pth);
        r2 <- d2.decode(r1.rest);
        r3 <- d3.decode(r2.rest);
        r4 <- d4.decode(r3.rest);
        r5 <- d5.decode(r4.rest);
        r6 <- d6.decode(r5.rest);
        r7 <- d7.decode(r6.rest);
        r8 <- d8.decode(r7.rest);
        r9 <- d9.decode(r8.rest);
        r10 <- d10.decode(r9.rest);
        r11 <- d11.decode(r10.rest);
        r12 <- d12.decode(r11.rest);
        r13 <- d13.decode(r12.rest);
        r14 <- d14.decode(r13.rest);
        r15 <- d15.decode(r14.rest);
        r16 <- d16.decode(r15.rest);
        r17 <- d17.decode(r16.rest);
        r18 <- d18.decode(r17.rest);
        r19 <- d19.decode(r18.rest);
        r20 <- d20.decode(r19.rest);
        r21 <- d21.decode(r20.rest)
      ) yield {
        r21.mapValue((v: T21) => (r1.value, r2.value, r3.value, r4.value, r5.value, r6.value, r7.value, r8.value, r9.value, r10.value, r11.value, r12.value, r13.value, r14.value, r15.value, r16.value, r17.value, r18.value, r19.value, r20.value, v))
      }
    }
  })

  implicit def matcher22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22] = apply[DecHPath22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22], (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22)]({ h =>
    lazy val d1 = matcher1[T1].decoder(h.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head)
    lazy val d2 = h.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.value
    lazy val d3 = h.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.value
    lazy val d4 = h.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.value
    lazy val d5 = h.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.value
    lazy val d6 = h.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.value
    lazy val d7 = h.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.value
    lazy val d8 = h.head.head.head.head.head.head.head.head.head.head.head.head.head.head.value
    lazy val d9 = h.head.head.head.head.head.head.head.head.head.head.head.head.head.value
    lazy val d10 = h.head.head.head.head.head.head.head.head.head.head.head.head.value
    lazy val d11 = h.head.head.head.head.head.head.head.head.head.head.head.value
    lazy val d12 = h.head.head.head.head.head.head.head.head.head.head.value
    lazy val d13 = h.head.head.head.head.head.head.head.head.head.value
    lazy val d14 = h.head.head.head.head.head.head.head.head.value
    lazy val d15 = h.head.head.head.head.head.head.head.value
    lazy val d16 = h.head.head.head.head.head.head.value
    lazy val d17 = h.head.head.head.head.head.value
    lazy val d18 = h.head.head.head.head.value
    lazy val d19 = h.head.head.head.value
    lazy val d20 = h.head.head.value
    lazy val d21 = h.head.value
    lazy val d22 = h.value
    PathDecoder { pth =>
      for (
        r1 <- d1.decode(pth);
        r2 <- d2.decode(r1.rest);
        r3 <- d3.decode(r2.rest);
        r4 <- d4.decode(r3.rest);
        r5 <- d5.decode(r4.rest);
        r6 <- d6.decode(r5.rest);
        r7 <- d7.decode(r6.rest);
        r8 <- d8.decode(r7.rest);
        r9 <- d9.decode(r8.rest);
        r10 <- d10.decode(r9.rest);
        r11 <- d11.decode(r10.rest);
        r12 <- d12.decode(r11.rest);
        r13 <- d13.decode(r12.rest);
        r14 <- d14.decode(r13.rest);
        r15 <- d15.decode(r14.rest);
        r16 <- d16.decode(r15.rest);
        r17 <- d17.decode(r16.rest);
        r18 <- d18.decode(r17.rest);
        r19 <- d19.decode(r18.rest);
        r20 <- d20.decode(r19.rest);
        r21 <- d21.decode(r20.rest);
        r22 <- d22.decode(r21.rest)
      ) yield {
        r22.mapValue((v: T22) => (r1.value, r2.value, r3.value, r4.value, r5.value, r6.value, r7.value, r8.value, r9.value, r10.value, r11.value, r12.value, r13.value, r14.value, r15.value, r16.value, r17.value, r18.value, r19.value, r20.value, r21.value, v))
      }
    }
  })

}