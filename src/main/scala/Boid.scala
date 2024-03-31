package main
import scalafx.scene.Scene
import scalafx.scene.canvas.Canvas
import scalafx.scene.paint.Color
import scalafx.scene.shape.{Polygon, Shape}

import scala.util.Random
import scala.math.{cos,sin,toRadians,acos}

// Coordinate system with operation
case class Point(x:Double, y:Double){
  def +(otherPoint:Point) = Point(x+otherPoint.x,y+otherPoint.y)
  def +(number:Double) = Point(x+number,y+number)
  def -(otherPoint:Point) = Point(x-otherPoint.x,y-otherPoint.y)
  def -(number:Double) = Point(x-number,y-number)
  def +-(number:Double) = Point(x+number,y-number)
  def -+(number:Double) = Point(x-number,y+number)
  def /(number:Double) = Point(x/number,y/number)
  def *(number:Double) = Point(x*number,y*number)
  def perpendicular = Point(y,-x)

  def minusx(otherPoint:Point) = Point(x-otherPoint.x,y)
  def minusy(otherPoint:Point) = Point(x,y-otherPoint.y)
  def minusx(value: Double) = Point(x - value, y)
  def minusy(value:Double) = Point(x, y-value)
  def plusx(otherPoint: Point) = Point(x + otherPoint.x, y)
  def plusy(otherPoint: Point) = Point(x, y + otherPoint.y)
  def plusx(value: Double) = Point(x + value, y)
  def plusy(value: Double) = Point(x, y + value)

  def dotProuct(other:Point)= x*other.x+y*other.y
  def distanceTo(other:Point)= math.sqrt((other.x-x)*(other.x-x)+(other.y-y)*(other.y-y))
  def unitVectorTowards(other: Point): Point = Point(other.x - this.x, other.y - this.y)./(distanceTo(other))
  def inverselyProportionalTo(to:Point,withValue:Double):Double = withValue/(this.distanceTo(to))

  def rotate(angle: Double): Point =
    val newX = x * cos(angle) - y * sin(angle)
    val newY = x * sin(angle) + y * cos(angle)
    Point(newX, newY)

}

class Boid(var pos:Point, var velocity:Point, World:world , var seperationWeight:Double = 10,var cohesionWeight: Double = 10,var fov:(Double) = 100.0, var foodWeight:Double = 40) {

  var maxSpeed:Double =  7
  var minSpeed: Double = 2
  var speed:Double = Random.between(4,maxSpeed)

  def setSeperation(value:Double) = seperationWeight=value
  def setCoherence(value:Double) = cohesionWeight= value
  def setFov(fovValue:Double) = fov=fovValue
  def setSpeed(b:Double)= this.speed=b
  def setMaxSpeed(v:Double) = maxSpeed=v
  var visibleBoids : Array[Boid] = Array[Boid]()
  var viewRange = 140

  def normalize(value:Double,min:Double,max:Double) = (value-min)/(max-min)

  def getColour:Color=
    val speed10 = speed*36
    val r=1-normalize(seperationWeight+fov,1,460)
    val g=normalize(cohesionWeight,1,100)
    val b=1-normalize(fov,1,360)
    Color(r,g,b,1)



  def updateVisibleBoids =    // todo   fov actual foviks pistetulolla ja inverse cosinilla
    var tmp=Array[Boid]()
    val angle=fov/2
    val unitVToDirection=pos.unitVectorTowards(velocity)
    for each <- World.listOfBoids do
      val unitVToOther=pos.unitVectorTowards(each.pos)
      val angleToOther=acos(unitVToDirection.dotProuct(unitVToOther))

      if each.pos != this.pos then
        if pos.distanceTo(each.pos) < viewRange then
          if angleToOther<angle then
            tmp=tmp.appended(each)
    visibleBoids=tmp

  def calculateFOVEndpoints: (Point, Point) = {
    val fovAngle = fov
    val fovAngleRad = toRadians(fovAngle / 2)

    val directionVector = pos.unitVectorTowards(velocity)

    val fovEndpointLeft = pos + directionVector.rotate(fovAngleRad) * viewRange
    val fovEndpointRight = pos + directionVector.rotate(-fovAngleRad) * viewRange

    (fovEndpointLeft, fovEndpointRight)
  }


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

  def limitWeights =
    if seperationWeight>100 then seperationWeight=100
    if seperationWeight<0 then seperationWeight=1
    if cohesionWeight>100 then cohesionWeight=100
    if cohesionWeight<0 then cohesionWeight=1
    if fov>360 then fov=360
    if fov<1 then fov=1

  def enforceSpeedLimits()=
    if speed > maxSpeed then setSpeed(maxSpeed)
    if speed < minSpeed then setSpeed(minSpeed)

  def moveTowardsFoods=
    for each <- World.listOfFoods do
      if pos.distanceTo(each.pos)<fov then velocity=pos.+(pos.unitVectorTowards(each.pos).*(foodWeight))
      if pos.distanceTo(each.pos)<2 then
        World.deleteFood(each)
        reproduce()




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
        pointForSeperation = pointForSeperation.-(pos.unitVectorTowards(each.pos))

    val seperation = if changed then pos.unitVectorTowards(pointForSeperation) else Point(0,0)
    val speed1 = speedSum / (amountOfBoids + 1)
    val cohesion = pos.unitVectorTowards(pointForCohesion./(amountOfBoids+1))

    (seperation, speed1, cohesion)



  def applyMovementRules() =
    if visibleBoids.length!=0 then
      val (seperationVector,newSpeed,cohesionVector) = getMovementVectors
      setSpeed(newSpeed)
      enforceSpeedLimits()
      velocity=velocity.+(cohesionVector.*(cohesionWeight)).+(seperationVector.*(seperationWeight))
      if pos.distanceTo(velocity)<speed then velocity=velocity.+(pos.unitVectorTowards(velocity).*(speed*2))

    else   // Boid moves in a straight line
      setSpeed((speed+Random.between(-0.2,0.2)))
      enforceSpeedLimits()
      val unitVectorTowardsVelocity = pos.unitVectorTowards(velocity).+-(World.seed.between(-0.2,0.2))  //adds some noise to movement
      velocity=velocity.+(unitVectorTowardsVelocity.*(speed))


  // Changes the boids location to a new one based on the velocity vector and its speed
  def move() =

    updateVisibleBoids
    applyMovementRules()
    if World.simulationWorldEnabled then moveTowardsFoods
    if pos.distanceTo(velocity) > 50 then velocity = pos.+((pos.unitVectorTowards(velocity)).*(50))
    val unitVectorScaled=pos.unitVectorTowards(velocity).*(speed)
    pos=pos.+(unitVectorScaled)
    moveAcrossFrame()


  def saveThis()=Boid(pos,velocity,World,seperationWeight,cohesionWeight,fov)

  def reproduce()=
    //evolution things go here
    var newSep=seperationWeight
    var newCoh=cohesionWeight
    var newFov= fov
    if World.seed.between(0,1)<World.mutationChance then
      newSep=newSep+World.seed.between(-10,10)
      newCoh=newCoh+World.seed.between(-10,10)
      newFov=newFov+World.seed.between(-20,20)
    val offspring=Boid(pos.+-(5),velocity.*(-1),World,newSep,newCoh,newFov)      //todo figure out how evolution actually works
    offspring.limitWeights
    World.spawnBoid(offspring)



  // should everything be rendered at the same time? if boids are affected as they go then the changes will affect the movement of others
  override def toString= s"Location: (${pos.x.round},${pos.y.round}) acceleration: (${velocity.x.round},${velocity.y.round}) speed: ${this.speed}  coh:${cohesionWeight}  sep: ${seperationWeight}  fov: $fov "
}

// todo: boid eating?, evolution?, reproduction?, seeking food?
//todo: io

class Food(var pos:Point,World:world){
  def removeIt=World.deleteFood(this)

  override def toString= s"Pos: ${pos}"
}
