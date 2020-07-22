package scalaphash

private[scalaphash] object MathUtils {
  type FloatMatrix = Array[Array[Float]]

  implicit class FloatMatrixOps(val matrix: FloatMatrix) extends AnyVal {
    def *(other: FloatMatrix): FloatMatrix = {
      if (matrix.length == 0 || matrix(0).length == 0 || matrix(0).length != other.length)
        throw new IllegalArgumentException(s"Can't multiply matrices")

      Array.tabulate(matrix.length, other(0).length) { (i, j) =>
        matrix(0).indices.foldLeft(0.0f)((res, k) => res + matrix(i)(k) * other(k)(j))
      }
    }

    /**
      * Crops matrix from x1 to x2 and y1 to y2 (inclusive)
      */
    def crop(x1: Int, y1: Int, x2: Int, y2: Int): FloatMatrix =
      Array.tabulate(x2 - x1 + 1, y2 - y1 + 1)((i, j) => matrix(i + x1)(j + y1))
  }

  implicit class FloatOps(val x: Float) extends AnyVal {
    def ~=(y: Float, delta: Float = 0.000001f): Boolean = y - delta <= x && x <= y + delta
  }
}
