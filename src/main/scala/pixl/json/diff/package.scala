package pixl.json

import io.circe.Json

package object diff {
  def mergeDeltas(jsons: Vector[Json]*): Json = {
    jsons.flatten.reduceLeft[Json]{
      case (a,b) if b.isNull => a
      case (a,b) if a.isNull => b
      case (a,b) => a.deepMerge(b)
    }
  }
}
