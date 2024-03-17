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

}

class Boid(var pos:Point, var acceleration:Point,World:world) {
  var maxSpeed = 7
  var speed:Double=Random.between(4,maxSpeed)
  var fov:(Double)= 100.0
  var seperationWeight:Double=30
  var coherenceWeight: Double=20
  var alignmentWeight:Double= 1

  var seperationActivationDistance=20

  def setSeperation(value:Double) = seperationWeight=value
  def setCoherence(value:Double) = coherenceWeight= value
  def setAlignment(value:Double) = alignmentWeight=value
  def setFov(fovValue:Double) = fov=fovValue
  def setSpeed(b:Double)= this.speed=b

  var visibleBoids : Array[Boid] = Array[Boid]()


  def updateVisibleBoids =    // this is a temporary solution, should be reworked at some point
    var tmp=Array[Boid]()
    for each <- World.listOfBoids do
      if each.pos != this.pos then
        if pos.distanceTo(each.pos) < fov then
          tmp=tmp.appended(each)
    visibleBoids=tmp


  // returns a unit vector for seperation, new value for speed, unit vector for cohesion

  def getMovementVectors: (Point,Double,Point) =
    var amountOfBoids = visibleBoids.length
    var pointForCohesion=pos

    var speedSum=0.0
    var pointForSeperation:Point=pos    // todo korjaa tää
    var changed= false

    for each <- visibleBoids do
      pointForCohesion=pointForCohesion.+(each.pos)
      speedSum+=each.speed

      if pos.distanceTo(each.pos) < fov/speed  then  // todo tää arvo
        changed=true
        pointForSeperation=pointForSeperation.-(each.pos)   // onko tää se hassu

    val seperation =if changed then  pos.unitVectorTowards(pointForSeperation) else Point(0,0)
    val speed1 = speedSum/amountOfBoids
    val cohesion = pos.unitVectorTowards(pointForCohesion./(amountOfBoids))

   /* println(s"pfc $pointForCohesion  +  pfa $pointForSeperation")
    println(amountOfBoids)
    println(pointForCohesion)
    println(s"$seperation +   ${pos.+(cohesion)}")*/
    (seperation,speed1,cohesion)


  // moves the boid across the edges of the frame, also shifts velocity vector accordingly
  def moveAcrossFrame(): Unit = {
  val maxX = World.windowWidth
  val maxY = World.windowHeight

  if (pos.x > maxX) then
    pos = Point(pos.x - maxX, pos.y)
    acceleration = Point(0, acceleration.y)

  if (pos.x < 0) then
    pos = Point(pos.x + maxX, pos.y)
    acceleration = Point(0, acceleration.y)

  if (pos.y > maxY) then
    pos = Point(pos.x, pos.y - maxY)
    acceleration = Point(acceleration.x, 0)

  if (pos.y < 0) then
    pos = Point(pos.x, pos.y + maxY)
    acceleration = Point(acceleration.x, 0)}

  /*  kept here just incase
  // returns a unit vector towards the average location of the boids visible to current boid.
  def cohesion: Point =
    var amount = visibleBoids.length
    var x = 0.0
    var y = 0.0
    for each <- visibleBoids do
      x += each.pos.x
      y += each.pos.y
    pos.unitVectorTowards(Point(x / amount, y / amount))
   */



  def applyMovementRules() =    //todo fix plz
    if visibleBoids.length!=0 then
      val (seperationVector,newSpeed,cohesionVector)= getMovementVectors
      if speed==newSpeed then setSpeed(speed+Random.between(-0.2,0.2)) else setSpeed(newSpeed)
      if speed>maxSpeed then setSpeed(maxSpeed)
      if seperationVector!=this.acceleration then acceleration=acceleration.+(seperationVector.*(seperationWeight))
      acceleration=acceleration.+(cohesionVector.*(coherenceWeight))

    else   // Boid moves in a straight line
      val unitVectorTowardsAcceleration =pos.unitVectorTowards(acceleration)
      acceleration=acceleration.+(unitVectorTowardsAcceleration)*speed



  // Changes the boids location to a new one based on the acceleration vector and its speed
  def move() =
    updateVisibleBoids
    applyMovementRules()
    val unitVectorScaled=pos.unitVectorTowards(acceleration).*(speed)
    pos=pos.+(unitVectorScaled)
    moveAcrossFrame()
    if pos.distanceTo(acceleration)!=100 then acceleration=pos.+(pos.unitVectorTowards(acceleration).*(100))

// should everything be rendered at the same time? if boids are affected as they go then the changes will affect the movement of others
  override def toString()=
    s"Location: (${pos.x.round},${pos.y.round}) acceleration: (${acceleration.x.round},${acceleration.y.round}) length: ${pos.distanceTo(acceleration)}  speed: ${this.speed}"
}

// todo: boid eating?, evolution?, reproduction?, seeking food?