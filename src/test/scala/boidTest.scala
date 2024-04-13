
package test
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import main.*

class boidTest extends AnyFlatSpec with Matchers:
  def test()=
  "boid" should "move correctly without any influences" in
    {
      val testWorld= World()
      val testBoid:Boid=Boid(Point(0,0),Point(5,5),testWorld)
      val destination = Point(1,1)
      testWorld.spawnBoid(testBoid)
      testBoid.move()
      testBoid.pos shouldEqual (destination)
    }

  it should "move away from other boid when they are close" in
  {
      val testWorld= World()
      val testBoid:Boid=Boid(Point(10,10),Point(11,11),testWorld)
      val otherTestBoid:Boid=Boid(Point(11,11),Point(12,12),testWorld)
      testWorld.spawnBoid(testBoid)
      testWorld.spawnBoid(otherTestBoid)
      testBoid.setSeperation(100)
      testWorld.tickFromWorld
      testBoid.pos.x should be < otherTestBoid.pos.x
      testBoid.pos.y should be < otherTestBoid.pos.y
      otherTestBoid.pos.x shouldNot be (0)
      otherTestBoid.pos.y shouldNot be (0)
  }

  it should "set speed to average of others" in {
    val testWorld = World()
    val testBoid: Boid = Boid(Point(10, 10), Point(11, 11), testWorld)
    val otherTestBoid: Boid = Boid(Point(11, 11), Point(12, 12), testWorld)
    testBoid.setSpeed(100)
    otherTestBoid.setSpeed(50)
    testWorld.tickFromWorld
    testBoid.setMaxSpeed(100)
    otherTestBoid.setMaxSpeed(100)
    testBoid.speed should be (75)
    otherTestBoid.speed should be(75)

  }






end boidTest


