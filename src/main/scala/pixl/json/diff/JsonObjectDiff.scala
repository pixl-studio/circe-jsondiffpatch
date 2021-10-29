package pixl.json.diff

import io.circe.syntax._
import io.circe.{Json, JsonObject}

object JsonObjectDiff {

  def diff(left:JsonObject, right:JsonObject): Json = {

    val leftElements = left.toVector
    val rightElements = right.toVector

    // compare left elements with right ones
    // remove if not present in right
    val leftDelta = leftElements.map { case (leftKey, leftValue) =>
      rightElements.collectFirst{ case (rightKey, rightValue) if leftKey == rightKey =>
        val o = JsonDiff.diff(leftValue, rightValue)
        if (!o.isNull){
          Json.obj(
            leftKey -> o
          )
        } else Json.obj()
      }.getOrElse {
        Json.obj(
          leftKey -> Json.arr(leftValue, 0.asJson, 0.asJson)
        )
      }
    }
    // add right elements not in left
    val rightDelta = rightElements.map { case (rightKey, rightValue) =>
      if (!leftElements.exists(_._1 == rightKey)){
        Json.obj(
          rightKey-> Json.arr(rightValue)
        )
      } else Json.obj()
    }

    mergeDeltas(leftDelta, rightDelta)
  }

  def isDiff(delta:Json): Boolean = {
    delta.isObject
  }
  def reverse(delta:Json): Json = {

    val o = delta.asObject.get

    val deltas = o.toVector.map { case (k, v) =>
      Json.obj(
        k -> JsonDiff.reverse(v)
      )
    }

    mergeDeltas(deltas)
  }

  def patch(left:Json, delta:Json): Json = {

    delta.asArray.map { items =>
      if (items.length == 1) {
        // add
        items.head
      } else if (items.length == 2){
        // replace
        items(1)
      }
      else {
        // delete -> add
        Json.Null
      }
    } getOrElse (Json.Null)
  }
}
