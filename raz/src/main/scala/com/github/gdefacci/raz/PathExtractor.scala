package com.github.gdefacci.raz

import scalaz.{\/, \/-}

trait PathExtractor[I] {
  
  def apply(i:I):Throwable \/ Path  
  
}

object PathExtractor {
  
  implicit val pathPathExtractor = new PathExtractor[Path] {
    def apply(i:Path):Throwable \/ Path = \/-(i)
  }
  
  
}
