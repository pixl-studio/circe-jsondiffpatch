package pixl.json.diff

import io.circe.Json
import io.circe.syntax._

object JsonDiff {

  def diff(left: Json, right: Json): Json = {

    (left, right) match {
      case (o1, o2) if left.isObject && right.isObject =>
        JsonObjectDiff.diff(o1.asObject.get, o2.asObject.get)

      case (o1, o2) if left.isArray && right.isArray =>
        JsonArrayDiff.diff(o1.asArray.get, o2.asArray.get)

      case (o1, o2) if left.isString && right.isString =>
        JsonStringDiff.diff(o1.asString.get, o2.asString.get)

      case _ =>
        Json.arr(left, right)
    }
  }

  def reverse(delta:Json): Json = {

    delta match {
      case d if JsonStringDiff.isDiff(d) =>
        JsonStringDiff.reverse(d)

      case d if JsonArrayDiff.isDiff(d) =>
        JsonArrayDiff.reverse(d)

      case d if JsonObjectDiff.isDiff(d) =>
        JsonObjectDiff.reverse(d)

      case _ => {
        delta.asArray.map { items =>
          if (items.length == 1) {
            // add -> delete
            Json.arr(items.head, 0.asJson, 0.asJson)

          } else if (items.length == 2){
            // replace
            Json.arr(items.reverse:_*)

          }
          else {
            // delete -> add
            Json.arr(items.head)
          }
        } getOrElse (Json.Null)
      }
    }
  }

  def patch(left:Json, delta:Json): Json = {

    delta match {
      case d if JsonStringDiff.isDiff(delta) =>
        JsonStringDiff.patch(left, d)

      case d if JsonArrayDiff.isDiff(delta) =>
        JsonArrayDiff.patch(left, d)

      case d if d.isArray =>
        JsonObjectDiff.patch(left, d)

      case d if d.isObject && left.isObject => {

        val ops = delta.asObject.get
        val items = left.asObject.get.toVector

        val deltas = ops.toVector.map { case (dk, dv) =>

          val o = items.collectFirst { case (k, v) if k == dk => v }.getOrElse(Json.obj())

          val newVal = JsonDiff.patch(o, dv)

          if (newVal.isNull){
            o.hcursor.downField(dk).delete.top.getOrElse(Json.obj())
          } else {
            Json.obj(
              dk -> newVal
            )
          }
        }

        deltas.reduceLeft(_ deepMerge _)
      }
    }

  }

  def unpatch(right:Json, delta:Json): Json = {
    patch(right, reverse(delta))
  }

}