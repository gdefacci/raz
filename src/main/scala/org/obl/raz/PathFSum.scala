package org.obl.raz

trait PathFSum {

  import HElems._
  import PathHelper._

  def pathFSum1[T1](h: HElem1[T1]): (T1) => Path = { (v) =>
    h.value(v)
  }

  def pathFSum2[T1, T2](h: HElem2[T1, T2]): ((T1, T2)) => Path = { (v) =>
    sum(pathFSum1(h.head)(v._1), h.value(v._2))
  }

  def pathFSum3[T1, T2, T3](h: HElem3[T1, T2, T3]): ((T1, T2, T3)) => Path = { (v) =>
    sum(pathFSum2(h.head)(v._1, v._2), h.value(v._3))
  }

  def pathFSum4[T1, T2, T3, T4](h: HElem4[T1, T2, T3, T4]): ((T1, T2, T3, T4)) => Path = { (v) =>
    sum(pathFSum3(h.head)(v._1, v._2, v._3), h.value(v._4))
  }

  def pathFSum5[T1, T2, T3, T4, T5](h: HElem5[T1, T2, T3, T4, T5]): ((T1, T2, T3, T4, T5)) => Path = { (v) =>
    sum(pathFSum4(h.head)(v._1, v._2, v._3, v._4), h.value(v._5))
  }

  def pathFSum6[T1, T2, T3, T4, T5, T6](h: HElem6[T1, T2, T3, T4, T5, T6]): ((T1, T2, T3, T4, T5, T6)) => Path = { (v) =>
    sum(pathFSum5(h.head)(v._1, v._2, v._3, v._4, v._5), h.value(v._6))
  }

  def pathFSum7[T1, T2, T3, T4, T5, T6, T7](h: HElem7[T1, T2, T3, T4, T5, T6, T7]): ((T1, T2, T3, T4, T5, T6, T7)) => Path = { (v) =>
    sum(pathFSum6(h.head)(v._1, v._2, v._3, v._4, v._5, v._6), h.value(v._7))
  }

  def pathFSum8[T1, T2, T3, T4, T5, T6, T7, T8](h: HElem8[T1, T2, T3, T4, T5, T6, T7, T8]): ((T1, T2, T3, T4, T5, T6, T7, T8)) => Path = { (v) =>
    sum(pathFSum7(h.head)(v._1, v._2, v._3, v._4, v._5, v._6, v._7), h.value(v._8))
  }

  def pathFSum9[T1, T2, T3, T4, T5, T6, T7, T8, T9](h: HElem9[T1, T2, T3, T4, T5, T6, T7, T8, T9]): ((T1, T2, T3, T4, T5, T6, T7, T8, T9)) => Path = { (v) =>
    sum(pathFSum8(h.head)(v._1, v._2, v._3, v._4, v._5, v._6, v._7, v._8), h.value(v._9))
  }

  def pathFSum10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10](h: HElem10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10]): ((T1, T2, T3, T4, T5, T6, T7, T8, T9, T10)) => Path = { (v) =>
    sum(pathFSum9(h.head)(v._1, v._2, v._3, v._4, v._5, v._6, v._7, v._8, v._9), h.value(v._10))
  }

  def pathFSum11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11](h: HElem11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11]): ((T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11)) => Path = { (v) =>
    sum(pathFSum10(h.head)(v._1, v._2, v._3, v._4, v._5, v._6, v._7, v._8, v._9, v._10), h.value(v._11))
  }

  def pathFSum12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12](h: HElem12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12]): ((T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12)) => Path = { (v) =>
    sum(pathFSum11(h.head)(v._1, v._2, v._3, v._4, v._5, v._6, v._7, v._8, v._9, v._10, v._11), h.value(v._12))
  }

  def pathFSum13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13](h: HElem13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13]): ((T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13)) => Path = { (v) =>
    sum(pathFSum12(h.head)(v._1, v._2, v._3, v._4, v._5, v._6, v._7, v._8, v._9, v._10, v._11, v._12), h.value(v._13))
  }

  def pathFSum14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14](h: HElem14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14]): ((T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14)) => Path = { (v) =>
    sum(pathFSum13(h.head)(v._1, v._2, v._3, v._4, v._5, v._6, v._7, v._8, v._9, v._10, v._11, v._12, v._13), h.value(v._14))
  }

  def pathFSum15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15](h: HElem15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15]): ((T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15)) => Path = { (v) =>
    sum(pathFSum14(h.head)(v._1, v._2, v._3, v._4, v._5, v._6, v._7, v._8, v._9, v._10, v._11, v._12, v._13, v._14), h.value(v._15))
  }

  def pathFSum16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16](h: HElem16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16]): ((T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16)) => Path = { (v) =>
    sum(pathFSum15(h.head)(v._1, v._2, v._3, v._4, v._5, v._6, v._7, v._8, v._9, v._10, v._11, v._12, v._13, v._14, v._15), h.value(v._16))
  }

  def pathFSum17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17](h: HElem17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17]): ((T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17)) => Path = { (v) =>
    sum(pathFSum16(h.head)(v._1, v._2, v._3, v._4, v._5, v._6, v._7, v._8, v._9, v._10, v._11, v._12, v._13, v._14, v._15, v._16), h.value(v._17))
  }

  def pathFSum18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18](h: HElem18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18]): ((T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18)) => Path = { (v) =>
    sum(pathFSum17(h.head)(v._1, v._2, v._3, v._4, v._5, v._6, v._7, v._8, v._9, v._10, v._11, v._12, v._13, v._14, v._15, v._16, v._17), h.value(v._18))
  }

  def pathFSum19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19](h: HElem19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]): ((T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19)) => Path = { (v) =>
    sum(pathFSum18(h.head)(v._1, v._2, v._3, v._4, v._5, v._6, v._7, v._8, v._9, v._10, v._11, v._12, v._13, v._14, v._15, v._16, v._17, v._18), h.value(v._19))
  }

  def pathFSum20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20](h: HElem20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20]): ((T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20)) => Path = { (v) =>
    sum(pathFSum19(h.head)(v._1, v._2, v._3, v._4, v._5, v._6, v._7, v._8, v._9, v._10, v._11, v._12, v._13, v._14, v._15, v._16, v._17, v._18, v._19), h.value(v._20))
  }

  def pathFSum21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21](h: HElem21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21]): ((T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21)) => Path = { (v) =>
    sum(pathFSum20(h.head)(v._1, v._2, v._3, v._4, v._5, v._6, v._7, v._8, v._9, v._10, v._11, v._12, v._13, v._14, v._15, v._16, v._17, v._18, v._19, v._20), h.value(v._21))
  }

  def pathFSum22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22](h: HElem22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22]): ((T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22)) => Path = { (v) =>
    sum(pathFSum21(h.head)(v._1, v._2, v._3, v._4, v._5, v._6, v._7, v._8, v._9, v._10, v._11, v._12, v._13, v._14, v._15, v._16, v._17, v._18, v._19, v._20, v._21), h.value(v._22))
  }

}

object PathFSum extends PathFSum 