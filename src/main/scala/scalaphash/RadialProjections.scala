package scalaphash

/**
  * Utilitary class for projections computing (Radial Hash algorithm)
  */
private[scalaphash] class RadialProjections(image: PixelMatrix, val projectionsCount: Int) {
  private lazy val Theta180 = Array.tabulate(180)(_ * Math.PI / 180)
  private lazy val TanTheta180 = Array.tabulate(180)(i => Math.tan(Theta180(i)))

  private lazy val maxSide = if (image.width > image.height) image.width else image.height
  private lazy val xOff =
    (image.width >> 1) + (image.width & 0x1) // round(image.getWidth/2) but only with integer operations
  private lazy val yOff =
    (image.height >> 1) + (image.height & 0x1) // round(image.getHeight/2) but only with integer operations

  val countPerLine: Array[Int] = Array.fill(projectionsCount)(0)
  val projections: Array[Array[Int]] = Array.fill(projectionsCount, maxSide)(0)

  compute()

  def maxDimension: Int = projections(0).length

  private def compute(): Unit = {
    computeFirstQuarter()
    computeLastQuarter()
  }

  private def computeFirstQuarter(): Unit =
    for {
      k <- 0 until (projectionsCount / 4 + 1)
      alpha = TanTheta180(k)
      x <- 0 until maxSide
    } {
      val y = alpha * (x - xOff)
      val yd = Math.floor(y + (if (y >= 0) 0.5 else -0.5)).toInt
      if ((yd + yOff >= 0) && (yd + yOff < image.height) && (x < image.width)) {
        projections(k)(x) = image.getY(x, yd + yOff)
        countPerLine(k) += 1
      }
      if ((yd + xOff >= 0) && (yd + xOff < image.width) && (k != projectionsCount / 4) && (x < image.height)) {
        projections(projectionsCount / 2 - k)(x) = image.getY(yd + xOff, x)
        countPerLine(projectionsCount / 2 - k) += 1
      }
    }

  private def computeLastQuarter(): Unit = {
    var j = 0
    for (k <- (3 * projectionsCount / 4) until projectionsCount) {
      val alpha = TanTheta180(k)
      for (x <- 0 until maxSide) {
        val y = alpha * (x - xOff)
        val yd = Math.floor(y + (if (y >= 0) 0.5 else -0.5)).toInt
        if ((yd + yOff >= 0) && (yd + yOff < image.height) && (x < image.width)) {
          projections(k)(x) = image.getY(x, yd + yOff)
          countPerLine(k) += 1
        }
        if (
          (yOff - yd >= 0)
          && (yOff - yd < image.width)
          && (2 * yOff - x >= 0)
          && (2 * yOff - x < image.height) && (k != 3 * projectionsCount / 4)
        ) {
          projections(k - j)(x) = image.getY(-yd + yOff, -(x - yOff) + yOff)
          countPerLine(k - j) += 1
        }
      }
      j += 2
    }
  }
}
