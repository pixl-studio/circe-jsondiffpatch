package pixl.json.diff

import io.circe.Json
import io.circe.syntax._
import org.bitbucket.cowwoc.diffmatchpatch.DiffMatchPatch

import java.util

object JsonStringDiff {

  val header = "^@@ +-(\\d+),(\\d+) +\\+(\\d+),(\\d+) +@@$".r

  def diff(left:String, right:String): Json = {
    if (left == right) Json.Null else {
      if (left.length < 60 && right.length < 60){
        Json.arr(
          left.asJson, right.asJson
        )
      } else {

        val dmp = new DiffMatchPatch()
        val diffs = dmp.patchMake(dmp.diffMain(left, right))

        Json.arr(
          dmp.patchToText(diffs).asJson, 0.asJson, 2.asJson
        )
      }
    }
  }

  def isDiff(delta:Json): Boolean = {
    delta.isArray && delta.asArray.exists(_.lift(2).exists(_.asNumber.exists(_.toInt.contains(2))))
  }

  def reverse(delta:Json): Json = {

    if (!isDiff(delta)) Json.Null else {

      val txt = delta.asArray.flatMap(_.lift(0).flatMap(_.asString)).getOrElse("")

      val lines = txt.split("\n")
      val reverseLines = lines.zipWithIndex.foldLeft(Array.empty[String]) { case (newlines, (line, i)) =>

        line.slice(0,1) match {
          case "@" => {
            newlines ++ header.findFirstMatchIn(line).map { m =>
              s"""@@ -${m.group(3)},${m.group(4)} +${m.group(1)},${m.group(2)} @@"""
            }
          }
          case "+" => {
            val l = '-' + line.substring(1)
            if (newlines.lastOption.exists(_.slice(0, 1) == "+")) {
              (newlines.slice(0, newlines.length-1) :+ l) ++ newlines.lastOption
            } else newlines :+ l
          }
          case "-" => {
            newlines :+ ('+' + line.substring(1))
          }
          case _ =>
            newlines :+ line
        }
      }

      Json.arr(
        reverseLines.mkString("\n").asJson,
        0.asJson, 2.asJson
      )
    }
  }

  def patch(left:Json, delta:Json): Json = {

    (for {
      s <- left.asString
      d <- delta.asArray
    } yield {

      val dmp = new DiffMatchPatch()
      val patchs = dmp.patchFromText(d.head.asString.getOrElse("")).asInstanceOf[util.LinkedList[DiffMatchPatch.Patch]]

      val result = dmp.patchApply(patchs, s)(0).asInstanceOf[String]

      result.asJson

    }).getOrElse(Json.Null)
  }
}
