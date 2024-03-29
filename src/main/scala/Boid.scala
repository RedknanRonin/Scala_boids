package main
import scalafx.scene.Scene
import scalafx.scene.canvas.Canvas
import scalafx.scene.paint.Color
import scalafx.scene.shape.{Polygon, Shape}
import scalafx.stage.Stage

import scala.annotation.targetName
import scala.util.Random

case class Point(x:Double, y:Double){
  def +(otherPoint:Point) = Point(x+otherPoint.x,y+otherPoint.y)
  def +(number:Double) = Point(x+number,y+number)
  def -(otherPoint:Point) = Point(x-otherPoint.x,y-otherPoint.y)
  def -(number:Double) = Point(x-number,y-number)
  def +-(number:Double) = Point(x+number,y-number)
  def -+(number:Double) = Point(x-number,y+number)
  def /(number:Double) = Point(x/number,y/number)
  def *(number:Double) = Point(x*number,y*number)

  def distanceTo(other:Point)= math.sqrt((other.x-x)*(other.x-x)+(other.y-y)*(other.y-y)) //math.sqrt(math.pow(other.x-this.x,2)+math.pow(other.y-this.y,2))
  def unitVectorTowards(other: Point): Point = Point(other.x - this.x, other.y - this.y)./(distanceTo(other))
  def inverselyProportionalTo(to:Point,withValue:Double):Double = withValue/(this.distanceTo(to))

}

class Boid(var pos:Point, var velocity:Point, World:world) {

  var maxSpeed:Double =  7
  var minSpeed: Double = 2
  var speed:Double = Random.between(4,maxSpeed)
  var fov:(Double) = 100.0
  var seperationWeight:Double = 10
  var cohesionWeight: Double = 10


  def setSeperation(value:Double) = seperationWeight=value
  def setCoherence(value:Double) = cohesionWeight= value
  def setFov(fovValue:Double) = fov=fovValue
  def setSpeed(b:Double)= this.speed=b
  def setMaxSpeed(v:Double) = maxSpeed=v

  var visibleBoids : Array[Boid] = Array[Boid]()


  def updateVisibleBoids =
    var tmp=Array[Boid]()
    for each <- World.listOfBoids do
      if each.pos != this.pos then
        if pos.distanceTo(each.pos) < fov then
          tmp=tmp.appended(each)
    visibleBoids=tmp



  // moves the boid across the edges of the frame, also shifts velocity vector accordingly
  def moveAcrossFrame(): Unit = {
  val maxX = World.windowWidth
  val maxY = World.windowHeight

  if (pos.x > maxX) then
    pos = Point(pos.x - maxX, pos.y)
    velocity = Point(velocity.x-maxX, velocity.y)

  if (pos.x < 0) then
    pos = Point(pos.x + maxX, pos.y)
    velocity = Point(velocity.x+maxX, velocity.y)

  if (pos.y > maxY) then
    pos = Point(pos.x, pos.y - maxY)
    velocity = Point(velocity.x, velocity.y-maxY)

  if (pos.y < 0) then
    pos = Point(pos.x, pos.y + maxY)
    velocity = Point(velocity.x, velocity.y+maxY)
   }



  def enforceSpeedLimits()=
    if speed > maxSpeed then setSpeed(maxSpeed)
    if speed < minSpeed then setSpeed(minSpeed)


  // returns a unit vector for cohesion, new value for speed and a seperation vector
  def getMovementVectors: (Point, Double, Point) =
    val amountOfBoids = visibleBoids.length
    var pointForCohesion = pos
    var speedSum = this.speed
    var pointForSeperation: Point = pos
    var changed=false

    for each <- visibleBoids do
      pointForCohesion = pointForCohesion.+(each.pos)
      speedSum += each.speed
      if pos.distanceTo(velocity)>fov/speed then
        changed=true
        pointForSeperation = pointForSeperation.-(pos.unitVectorTowards(each.pos))//.*(pos.inverselyProportionalTo(each.pos,seperationWeight)))

    val seperation = if changed then pos.unitVectorTowards(pointForSeperation) else Point(0,0)
    val speed1 = speedSum / (amountOfBoids + 1)
    val cohesion = pos.unitVectorTowards(pointForCohesion./(amountOfBoids+1))

    (seperation, speed1, cohesion)


  def applyMovementRules() =
    if visibleBoids.length!=0 then
      val (seperationVector,newSpeed,cohesionVector) = getMovementVectors
      setSpeed(newSpeed)
      enforceSpeedLimits()
      velocity=velocity.+(cohesionVector.*(cohesionWeight)).+(seperationVector.*(seperationWeight))  // todo boids move to the right when flocking
      if pos.distanceTo(velocity)<speed then velocity=velocity.+(pos.unitVectorTowards(velocity).*(speed))

    else   // Boid moves in a straight line
      setSpeed((speed+Random.between(-0.2,0.2)))
      enforceSpeedLimits()
      val unitVectorTowardsVelocity = pos.unitVectorTowards(velocity).+-(World.seed.between(-0.2,0.2))  //adds some noise to movement
      velocity=velocity.+(unitVectorTowardsVelocity.*(speed))




  // Changes the boids location to a new one based on the velocity vector and its speed
  def move() =
    updateVisibleBoids
    applyMovementRules()
    if pos.distanceTo(velocity) > 50 then velocity = pos.+((pos.unitVectorTowards(velocity)).*(50))
    val unitVectorScaled=pos.unitVectorTowards(velocity).*(speed)
    pos=pos.+(unitVectorScaled)
    moveAcrossFrame()


  // should everything be rendered at the same time? if boids are affected as they go then the changes will affect the movement of others
  override def toString= s"Location: (${pos.x.round},${pos.y.round}) acceleration: (${velocity.x.round},${velocity.y.round}) length: ${pos.distanceTo(velocity)}  speed: ${this.speed}"
}

// todo: boid eating?, evolution?, reproduction?, seeking food?
