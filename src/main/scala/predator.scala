package main
import main.*

import scala.math.acos
import scala.util.Random
class Predator(pos:Point, velocity:Point, World:world , seperationWeight:Double = 400,cohesionWeight: Double = 10,fov:(Double))
  extends Boid(pos, velocity, World , seperationWeight ,cohesionWeight,fov){

  //predators can either seek the middle of the group,  or singlular boids
  // aversion to other predators

  maxSpeed=6
  minSpeed=3
  var pVelocity=velocity
  var pPos=pos
  var pFov = fov

  var lifetime =  400
  var lifetimeCounter = 0

  predatorAversionWeight = 20
  var preyCohesionWeight: Double = 5

  def incrementLifetimeCounter =
    lifetimeCounter+=1
    if lifetimeCounter==lifetime then
      if World.listOfPredators.length!=1 then World.listOfPredators-=this

  override def moveAcrossFrame(): Unit = {
    val maxX = World.windowWidth
    val maxY = World.windowHeight

    if (pPos.x > maxX) then
      pPos = Point(pPos.x - maxX, pPos.y)
      pVelocity = Point(pVelocity.x - maxX, pVelocity.y)

    if (pPos.x < 0) then
      pPos = Point(pPos.x + maxX, pPos.y)
      pVelocity = Point(pVelocity.x + maxX, pVelocity.y)

    if (pPos.y > maxY) then
      pPos = Point(pPos.x, pPos.y - maxY)
      pVelocity = Point(pVelocity.x, pVelocity.y - maxY)

    if (pPos.y < 0) then
      pPos = Point(pPos.x, pPos.y + maxY)
      pVelocity = Point(pVelocity.x, pVelocity.y + maxY)

  }

  override def calculateFOVEndpoints: (Point, Point) = {
    val fovAngle = pFov
    val fovAngleRad = scala.math.toRadians(fovAngle / 2)

    val directionVector = pPos.unitVectorTowards(pVelocity)

    val fovEndpointLeft = pPos + directionVector.rotate(fovAngleRad) * viewRange
    val fovEndpointRight = pPos + directionVector.rotate(-fovAngleRad) * viewRange

    (fovEndpointLeft, fovEndpointRight)
  }

  override def updateVisibleBoids =
    var tmp=Array[Boid]()
    val angle=fov/2
    val unitVToDirection=pPos.unitVectorTowards(velocity)
    for each <- World.listOfBoids do
      val unitVToOther=pPos.unitVectorTowards(each.pos)
      val angleToOther=acos(unitVToDirection.dotProuct(unitVToOther))

      if each.pos != this.pPos then
        if pPos.distanceTo(each.pos) < viewRange then
          if angleToOther<angle then
            tmp=tmp.appended(each)
    visibleBoids=tmp

  override def getMovementVectors: (Point, Double,Point) =
    val amountOfPredators = visiblePredators.length
    var speedSum = this.speed
    var pointForSeperation: Point = pPos
    var changed=false

    for each <- visiblePredators do
      speedSum += each.speed
      if pPos.distanceTo(each.pPos)>pFov/speed then
        changed=true
        pointForSeperation = pointForSeperation.-(pPos.unitVectorTowards(each.pPos))

    val seperation = if changed then pPos.unitVectorTowards(pointForSeperation) else Point(0,0)
    val speed1 = speedSum / (amountOfPredators + 1)
    (seperation, speed1,Point(0,0))

  override def applyMovementRules() =
    if visiblePredators.length!=0 then
      val (seperationVector,newSpeed,empty) = getMovementVectors
      setSpeed(newSpeed)
      enforceSpeedLimits()
      pVelocity=pVelocity.+(seperationVector.*(seperationWeight))
      if pPos.distanceTo(pVelocity)<speed then pVelocity=pVelocity.+(pPos.unitVectorTowards(pVelocity).*(speed*2))

    else   // moves in a straight line
        setSpeed((speed+Random.between(-0.2,0.2)))
        enforceSpeedLimits()
        val unitVectorTowardsVelocity = pPos.unitVectorTowards(pVelocity).+-(World.seed.between(-0.2,0.2))  //adds some noise to movement
        pVelocity=pVelocity.+(unitVectorTowardsVelocity.*(speed))

  override  def reproduce() =
    var newSep = seperationWeight
    var newCoh = cohesionWeight
    var newFov = fov
    if World.seed.between(0, 1) < World.mutationChance then
      newSep = newSep + World.seed.between(-10, 10)
      newCoh = newCoh + World.seed.between(-10, 10)
      newFov = newFov + World.seed.between(-20, 20)
    val offspring = Predator(pPos.+-(5),pVelocity.*(-1), World, newSep, newCoh, newFov)
    offspring.limitWeights
    World.spawnPredator(offspring)

  override def moveTowardsFoods =
    for each <- visibleBoids do
      pVelocity = pVelocity.+(pPos.unitVectorTowards(each.pos).*(foodWeight))
      if pPos.distanceTo(each.pos) < 3 then
        World.deleteBoid(each)
        reproduce()


  override def move() =
    updateVisibleBoids
    updateVisiblePredators
    moveTowardsFoods
    applyMovementRules()
    if pPos.distanceTo(pVelocity) > 50 then pVelocity = pPos.+((pPos.unitVectorTowards(pVelocity)).*(50))
    val unitVectorScaled=pPos.unitVectorTowards(pVelocity).*(speed)
    pPos=pPos.+(unitVectorScaled)
    moveAcrossFrame()
    incrementLifetimeCounter


  override def toString = s"Predator\nLocation: (${pPos.x.round},${pPos.y.round}) acceleration: (${pVelocity.x.round},${pVelocity.y.round}) speed: ${this.speed}  coh:${cohesionWeight}  sep: ${seperationWeight}  fov: $fov "

}
