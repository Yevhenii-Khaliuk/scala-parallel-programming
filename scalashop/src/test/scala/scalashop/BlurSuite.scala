package scalashop

import java.util.concurrent.*
import scala.collection.*

class BlurSuite extends munit.FunSuite :

  trait TestSet:
    /**
     * Image:
     * 1 2 3 4
     * 4 5 6 7
     * 4 2 3 1
     * 1 5 3 2
     */
    private val data = Array(1, 2, 3, 4, 4, 5, 6, 7, 4, 2, 3, 1, 1, 5, 3, 2)
    val srcImg = new Img(4, 4, data)

  test("boxBlurKernel: (0, 0), rad = 1") {
    new TestSet:
      assertEquals(boxBlurKernel(srcImg, 0, 0, 1), 3)
  }

  test("boxBlurKernel: (3, 3), rad = 1") {
    new TestSet:
      assertEquals(boxBlurKernel(srcImg, 3, 3, 1), 2)
  }

  test("boxBlurKernel: (1, 1), rad = 1") {
    new TestSet:
      assertEquals(boxBlurKernel(srcImg, 1, 1, 1), 3)
  }
