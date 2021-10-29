package pixl.json.diff

import io.circe.{Json, JsonObject}
import io.circe.syntax._

object JsonArrayDiff {

  def diff(left: Vector[Json], right: Vector[Json]):Json = {
    if (left == right) Json.Null else {

      val leftIndexed = left.zipWithIndex
      val rightIndexed = right.zipWithIndex

      val rightDelta = rightIndexed.map { case (r, rindex) =>
        if (leftIndexed.exists { case (l, lindex) => l == r }) Json.obj() else {

          leftIndexed.lift(rindex).map { case (l, lindex) =>
            Json.obj(s"$rindex" -> JsonDiff.diff(l, r))
          } getOrElse {
            Json.obj(s"$rindex" -> Json.arr(r))
          }
        }
      }

      val leftDelta = leftIndexed.map { case (l, lindex) =>
        rightIndexed.collectFirst { case (r,rindex) if r == l =>
          if (lindex == rindex) Json.Null else {
            Json.obj(s"_$lindex" -> Json.arr("".asJson, rindex.asJson , 3.asJson))
          }
        } getOrElse {
          if(rightDelta.exists(_.asObject.exists(_.keys.exists(_ == s"$lindex")))) Json.obj() else {
            Json.obj(s"_$lindex" -> Json.arr("d".asJson, 0.asJson , 0.asJson))
          }
        }
      }

      val delta = mergeDeltas(leftDelta, rightDelta)

      if (delta == Json.obj()) Json.Null else {
        Json.obj("_t" -> "a".asJson).deepMerge(delta)
      }
    }
  }

  def isDiff(delta:Json):Boolean = {
    delta.isObject && delta.asObject.exists { o =>
      o.toVector.exists { case  (k,v) => k == "_t" && v.asString.contains("a") }
    }
  }

  def reverse(delta:Json): Json = {

    if (!isDiff(delta)) Json.Null else {

      val o = delta.asObject.get

      val deltas = o.toVector.collect { case (k,v) if k != "_t" =>

        val a = v.asArray.getOrElse(Vector.empty[Json])

        if (k.startsWith("_")){

          val kIndex = k.substring(1)

          a.lift(2).flatMap(_.asNumber.flatMap(_.toInt)).collectFirst { case n if n == 3 =>
            // move
            val oldkey = a.lift(1).flatMap(_.asNumber.flatMap(_.toInt.map(_.toString))).getOrElse("")

            Json.obj(
              "_"+oldkey -> Json.arr(a.head, kIndex.toInt.asJson, 3.asJson)
            )
          } getOrElse {
            // remove -> add
            Json.obj(kIndex -> Json.arr(a.head))
          }

        } else {
          // add -> remove
          Json.obj("_" + k -> Json.arr(a.head, 0.asJson, 0.asJson))
        }
      }

      Json.obj("_t" -> "a".asJson)
        .deepMerge(mergeDeltas(deltas))
    }
  }

  def patch(left: Json, delta:Json): Json = {

    val newItems =
      delta.asObject.map(_.toVector.sortWith{ case ((ak, av), (bk, bv)) =>

        val aItems = av.asArray.getOrElse(Vector.empty)
        val bItems = av.asArray.getOrElse(Vector.empty)

        val aIsMove = aItems.length == 3 && aItems.lift(2).exists(_.asNumber.contains(3))
        //val bIsMove = bItems.length == 3 && aItems.lift(2).exists(_.asNumber.contains(3))
        val aIsDelete = aItems.length == 3 && !aItems.lift(2).exists(_.asNumber.contains(3))
        val bIsDelete = bItems.length == 3 && !aItems.lift(2).exists(_.asNumber.contains(3))

        if (aIsDelete){
          true
        } else if (aIsMove && bIsDelete) {
          false
        } else if (aIsMove) {
          true
        } else false

      }).getOrElse(Vector.empty)
      .foldLeft(left.asArray.getOrElse(Vector.empty[Json])) {
        case (items, (dk, dv)) =>

          if (dk == "_t") items else {

          val dItems = dv.asArray.getOrElse(Vector.empty[Json])

          if (dk.startsWith("_") && dItems.length == 3 && dItems(2).asNumber.exists(_.toInt.contains(3))){

            (for {
              oldindex <- dk.substring(1).toIntOption
              newindex <- dv.asArray.getOrElse(Vector.empty[Json])(1).asNumber.flatMap(_.toInt)
              oldvalue <- items.lift(oldindex)
              newvalue <- items.lift(newindex)
            } yield {

              items
                .updated(oldindex, newvalue)
                .updated(newindex, oldvalue)

            }).getOrElse(Vector.empty[Json])

          } else if (dk.startsWith("_")){

            items.patch(
              dk.substring(1).toInt, Nil, 1
            )

          } else {
            items.patch(
              dk.toInt, List(dItems(0)), 0
            )
          }
        }
    }

    Json.arr(newItems:_*)
  }
}
