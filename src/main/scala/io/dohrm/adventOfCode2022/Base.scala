package io.dohrm.adventOfCode2022
import java.io.FileReader
import java.nio.charset.StandardCharsets
import java.nio.file.Path
import scala.io.Source
import scala.reflect.ClassTag

trait Base { self =>
  def read: String = {
    val source = Source.fromFile(s"data/${self.getClass.getSimpleName.toLowerCase.replaceAll("\\$", "")}.txt")
    try {
      source.getLines().mkString("\n")
    } finally {
      source.close()
    }
  }

}
