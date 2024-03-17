
package test
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import main.*

class boidTest extends AnyFlatSpec with Matchers:
  def test()=
  "boid" should "move correctly without any influences" in
    {
      val testWorld= world()
      val testBoid:Boid=Boid(Point(0,0),Point(5,5),testWorld)
      val destination = Point(1,1)
      testWorld.spawnBoid(testBoid)
      testBoid.move()
      testBoid.pos shouldEqual (destination)
    }

  it should "move away from other boid when they are close" in
  {
    val testWorld= world()
      val testBoid:Boid=Boid(Point(10,10),Point(11,11),testWorld)
      val otherTestBoid:Boid=Boid(Point(11,11),Point(12,12),testWorld)
      testWorld.spawnBoid(testBoid)
      testBoid.setSeperation(100)
      testWorld.tickForTest
      testBoid.pos.x should be < otherTestBoid.pos.x
      testBoid.pos.y should be < otherTestBoid.pos.y
      otherTestBoid.pos.x shouldNot be (0)
      otherTestBoid.pos.y shouldNot be (0)
  }






end boidTest


