package io.dohrm.adventOfCode

import java.io.FileReader
import java.nio.charset.StandardCharsets
import java.nio.file.Path
import scala.io.Source
import scala.reflect.ClassTag

trait Base { self =>
  def year: String
  def read: String = {
    val source =
      Source.fromFile(s"data/adventOfCode$year/${self.getClass.getSimpleName.toLowerCase.replaceAll("\\$", "")}.txt")
    try {
      source.getLines().mkString("\n")
    } finally {
      source.close()
    }
  }

}
