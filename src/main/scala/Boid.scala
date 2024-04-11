package main
import scalafx.scene.paint.Color

import java.util.ConcurrentModificationException
import scala.util.Random
import scala.math.{acos, cos, sin, toRadians}


// Coordinate system with operations
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

class Boid(var pos:Point, var velocity:Point, World:world , var seperationWeight:Double = 10,var cohesionWeight: Double = 10,var fov:(Double) = 100.0, var foodWeight:Double = 10, var predatorAversionWeight:Double=10) {
  var maxSpeed:Double =  7
  var minSpeed: Double = 2
  var speed:Double = Random.between(4,maxSpeed)   // the speed of a boid is random when spawning

  def setSeperation(value:Double) = seperationWeight=value
  def setCoherence(value:Double) = cohesionWeight= value
  def setFov(fovValue:Double) = fov=fovValue
  def setSpeed(b:Double)= this.speed=b
  def setMaxSpeed(v:Double) = maxSpeed=v
  def setFoodWeight(value:Double)= foodWeight=value
  def setPredatorAversion(value:Double)=predatorAversionWeight=value

  var visibleBoids : Array[Boid] = Array[Boid]()
  var visiblePredators = Array[Predator]()
  var viewRange = 140

  def normalize(value:Double,min:Double,max:Double) = (value-min)/(max-min)  //function to set value between 0 and 1 for colours.

  //the colour of a boid is affected by its parameters. This differentiates boids visually according to their parameters.
  def getColour:Color=
    limitWeights
    val speed10 = speed*36
    val r=1-normalize(seperationWeight+fov/180,0,12)
    val g=normalize(cohesionWeight,0,10)
    val b=normalize(fov,1,360)
    Color(r,g,b,1)

  // iterates list of all predators to check if any are in sight and updates list.
  def updateVisiblePredators =
    var tmp = Array[Predator]()
    val angle = fov / 2
    val unitVToDirection = pos.unitVectorTowards(velocity)
    for each <- World.listOfPredators do
      val unitVToOther = pos.unitVectorTowards(each.pPos)
      if pos.distanceTo(each.pPos) < viewRange then
        val angleToOther = acos(unitVToDirection.dotProuct(unitVToOther))
        if angleToOther < angle then
              tmp = tmp.appended(each)
    visiblePredators = tmp

  // iterates list of all boids to check if any are in sight and updates list
  def updateVisibleBoids =
    var tmp=Array[Boid]()
    val angle=fov/2
    val unitVToDirection=pos.unitVectorTowards(velocity)
    for each <- World.listOfBoids do
      val unitVToOther=pos.unitVectorTowards(each.pos)
      if each.pos != this.pos then
        if pos.distanceTo(each.pos) < viewRange then
          val angleToOther=acos(unitVToDirection.dotProuct(unitVToOther))
            if angleToOther<angle then
              tmp=tmp.appended(each)
    visibleBoids=tmp

  //returns endpoints for drawing field of view lines.
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

  //returns the values of a boids weights to within bounds
  def limitWeights =
    if seperationWeight>10 then seperationWeight=10
    if seperationWeight<0 then seperationWeight=0
    if cohesionWeight>10 then cohesionWeight=10
    if cohesionWeight<0 then cohesionWeight=0
    if fov>360 then fov=360
    if fov<1 then fov=1


  def enforceSpeedLimits()=
    if speed > maxSpeed then setSpeed(maxSpeed)
    if speed < minSpeed then setSpeed(minSpeed)

  //moves boid towards food and handles eating/reproduction
  def moveTowardsFoods=
    for each <- World.listOfFoods do
      if pos.distanceTo(each.pos)<fov then velocity=pos.+(pos.unitVectorTowards(each.pos).*(foodWeight))
      if pos.distanceTo(each.pos)<2 then
        World.deleteFood(each)
        scala.util.control.Exception.ignoring(classOf[ConcurrentModificationException]){
        reproduce()}


  // returns a unit vector to avoid predators
  def unitVectorToAvoidPredators: Point =
    var startPoint = pos
    var changed=false  //else divides by 0
    for each <- World.listOfPredators do
      if visiblePredators.contains(each) then
        startPoint = startPoint.-(pos.unitVectorTowards(each.pPos))
        changed = true
    if changed then pos.unitVectorTowards(startPoint) else Point(0,0)

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
        pointForSeperation = pointForSeperation.-(pos.unitVectorTowards(each.pos).*(2))

    val seperation = if changed then pos.unitVectorTowards(pointForSeperation) else Point(0,0)
    val newSpeed = speedSum / (amountOfBoids + 1)
    val cohesion = pos.unitVectorTowards(pointForCohesion./(amountOfBoids+1))

    (seperation, newSpeed, cohesion)


    // applies movement rules to boid, meaning updates the velocity vector according to movement rules
  def applyMovementRules() =
    if visibleBoids.length!=0 then
      val (seperationVector,newSpeed,cohesionVector) = getMovementVectors
      val avoidPredators=unitVectorToAvoidPredators
      setSpeed(newSpeed)
      enforceSpeedLimits()
      velocity=velocity.+(cohesionVector.*(cohesionWeight)).+(seperationVector.*(seperationWeight*2)).+(avoidPredators.*(predatorAversionWeight))
      if pos.distanceTo(velocity)<speed then velocity=velocity.+(pos.unitVectorTowards(velocity).*(speed*2))

    else   // Boid moves in a straight line with some noise
      setSpeed((speed+Random.between(-0.2,0.2)))
      enforceSpeedLimits()
      val avoidPredatorsNoBoids=unitVectorToAvoidPredators
      velocity=velocity.+(avoidPredatorsNoBoids.*(predatorAversionWeight))

      val unitVectorTowardsVelocity = pos.unitVectorTowards(velocity).+-(World.seed.between(-0.2,0.2))  //adds some noise to movement
      velocity=velocity.+(unitVectorTowardsVelocity.*(speed))


  // Changes the boids location to a new one based on the velocity vector and its speed
  def move() =
    updateVisibleBoids
    applyMovementRules()
    if World.simulationWorldEnabled then moveTowardsFoods
    if pos.distanceTo(velocity) > 50 then velocity = pos.+((pos.unitVectorTowards(velocity)).*(50)) // limits velocity vector length
    val unitVectorScaled=pos.unitVectorTowards(velocity).*(speed)
    pos=pos.+(unitVectorScaled)
    moveAcrossFrame()

  // produces another boid that can have changes in its weights
  def reproduce()=
    var newSep=seperationWeight
    var newCoh=cohesionWeight
    var newFov= fov
    var newFood=foodWeight
    var newPAversion=predatorAversionWeight
    if World.seed.between(0,1)<World.mutationChance then
      if World.seed.nextBoolean() then newSep=newSep+World.seed.between(-2,2)
      if World.seed.nextBoolean() then newCoh=newCoh+World.seed.between(-2,2)
      if World.seed.nextBoolean() then newFov=newFov+World.seed.between(-20,20)
      if World.seed.nextBoolean() then newFood=newFood+World.seed.between(-2,2)
      if World.seed.nextBoolean() then newPAversion = newPAversion + World.seed.between(-3, 3)

    val offspring=Boid(pos.+-(5),velocity.*(-1),World,newSep,newCoh,newFov,newFood,newPAversion)
    World.spawnBoid(offspring)

  //used in saving and loading
  override def toString= s"${pos.x},${pos.y},${velocity.x},${velocity.y},$seperationWeight,$cohesionWeight,$fov,$foodWeight,$predatorAversionWeight"
}


class Food(var pos:Point,World:world){
  def removeIt=World.deleteFood(this)

  override def toString= s"${pos.x},${pos.y}"
}
