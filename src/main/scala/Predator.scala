package main
import main.*

import scala.math.acos
import scala.util.Random
class Predator(pos:Point, velocity:Point, world:World, seperationWeight:Double = 10, cohesionWeight: Double = 8, fov:(Double),var foodweight:Int=5)
  extends Boid(pos, velocity, world , seperationWeight ,cohesionWeight,fov){

  //predators are much like boids but with some alterations to movement
  // they've got their own p(redator)pos and other variables to seperate them from boids?

  maxSpeed=5  //The max speed of predators is smaller than that of Boids
  minSpeed=3
  var pVelocity=velocity
  var pPos=pos
  var pFov = fov

  //predators have a certain lifetime after which they die unless they are the only predator alive
  var lifetime =  400
  var lifetimeCounter = 0

  predatorAversionWeight = 20
  var preyCohesionWeight: Double = 5

  def incrementLifetimeCounter =
    lifetimeCounter+=1
    if lifetimeCounter==lifetime then
      if world.listOfPredators.length!=1 then world.listOfPredators-=this

  override def moveAcrossFrame(): Unit = {
    val maxX = world.windowWidth
    val maxY = world.windowHeight

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
    var speedSum = this.speed
    var tmp=Array[Boid]()
    val angle=fov/2
    val unitVToDirection=pPos.unitVectorTowards(pVelocity)
    for each <- world.listOfBoids do
      val unitVToOther=pPos.unitVectorTowards(each.pos)

      if each.pos != this.pPos then
        if pPos.distanceTo(each.pos) < viewRange then
          val angleToOther=acos(unitVToDirection.dotProuct(unitVToOther))
            if angleToOther<angle then
              speedSum += each.speed
              tmp=tmp.appended(each)
    visibleBoids=tmp
    if visibleBoids.length!=0 then setSpeed(speedSum/visibleBoids.length)

  override def updateVisiblePredators =
    var tmp = Array[Predator]()
    val angle = pFov / 2
    val unitVToDirection = pPos.unitVectorTowards(pVelocity)
    for each <- world.listOfPredators do
      val unitVToOther = pPos.unitVectorTowards(each.pPos)
      if pPos.distanceTo(each.pPos) < viewRange then
        val angleToOther = acos(unitVToDirection.dotProuct(unitVToOther))
        if angleToOther < angle then
          tmp = tmp.appended(each)
    visiblePredators = tmp

  override def getMovementVectors: (Point, Double,Point,Point) =
    val amountOfPredators = visiblePredators.length
    var pointForSeperation: Point = pPos
    var pointForCohesion = pPos
    var changed=false
    for each <- visiblePredators do
      pointForCohesion.+(each.pPos)
      if pPos.distanceTo(each.pPos)<50 then
        changed=true
        pointForSeperation = pointForSeperation.-(pPos.unitVectorTowards(each.pPos).*(5))

    val seperation = if changed then pPos.unitVectorTowards(pointForSeperation) else Point(0,0)
    val cohesion = pPos.unitVectorTowards(pointForCohesion./(amountOfPredators+1))

    (seperation, 0,cohesion,cohesion)


  override def applyMovementRules() =
    if visiblePredators.length!=0 then
      val (seperationVector,zero,cohesionVector,nothing) = getMovementVectors
      enforceSpeedLimits()
      pVelocity=pVelocity.+(seperationVector.*(seperationWeight))//.+((cohesionVector.*(cohesionWeight)))
      if pPos.distanceTo(pVelocity)<speed then pVelocity=pVelocity.+(pPos.unitVectorTowards(pVelocity).*(speed*2))

    else   // moves in a straight line
        setSpeed((speed+Random.between(-0.2,0.2)))
        enforceSpeedLimits()
        val unitVectorTowardsVelocity = pPos.unitVectorTowards(pVelocity).+-(world.seed.between(-0.2,0.2))  //adds some noise to movement
        pVelocity=pVelocity.+(unitVectorTowardsVelocity.*(speed))


  override  def reproduce() =
    var newSep = seperationWeight
    var newCoh = cohesionWeight
    var newFov = fov
    if world.seed.between(0, 1) < world.mutationChance then
      if world.seed.nextBoolean() then newSep = newSep + world.seed.between(-10, 10)
      if world.seed.nextBoolean() then newCoh = newCoh + world.seed.between(-10, 10)
      if world.seed.nextBoolean() then newFov = newFov + world.seed.between(-20, 20)
    val offspring: Predator = Predator(pPos.+-(5), pVelocity.*(-1), world, newSep, newCoh, newFov)
    offspring.limitWeights
    world.spawnPredator(offspring)
  end reproduce



  override def moveTowardsFoods =
    if visibleBoids.length!=0 then
      val closest=visibleBoids.minBy(_.pos.distanceTo(pPos))
        pVelocity = pVelocity.+(pPos.unitVectorTowards(closest.pos).*(foodweight))
        if pPos.distanceTo(closest.pos) < 5 then
          world.deleteBoid(closest)
          reproduce()


  override def move() =
    updateVisibleBoids
    updateVisiblePredators
   // moveTowardsFoods   //moves towards boids first
    applyMovementRules()  // then avoids others
    moveTowardsFoods
    if pPos.distanceTo(pVelocity) > speed/viewRange then pVelocity = pPos.+((pPos.unitVectorTowards(pVelocity)).*(50))
    val unitVectorScaled=pPos.unitVectorTowards(pVelocity).*(speed)
    pPos=pPos.+(unitVectorScaled)
    moveAcrossFrame()
    incrementLifetimeCounter

  override def toString = s"${pos.x},${pos.y},${velocity.x},${velocity.y},$seperationWeight,$cohesionWeight,$fov"

}




