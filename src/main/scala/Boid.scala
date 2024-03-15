package main
import scalafx.scene.Scene
import scalafx.scene.canvas.Canvas
import scalafx.scene.paint.Color
import scalafx.scene.shape.{Polygon, Shape}
import scalafx.stage.Stage

import scala.annotation.targetName

case class Point(x:Double, y:Double){
  def +(otherPoint:Point) = Point(x+otherPoint.x,y+otherPoint.y)
  def +(number:Double) = Point(x+number,y+number)
  def -(otherPoint:Point) = Point(x-otherPoint.x,y-otherPoint.y)
  def /(number:Double) = Point(x/number,y/number)
  def *(number:Double) = Point(x*number,y*number)

  def distanceTo(other:Point)= math.sqrt(math.pow(other.x-this.x,2)+math.pow(other.y-this.y,2))
  def unitVectorTowards(other: Point): Point = other.-(this)./(this.distanceTo(other))

}

class Boid(var pos:Point, var acceleration:Point,World:world) {
  var speed:Double=5
  var fov:(Double)= 100.0
  var seperationWeight=10.0
  var coherenceWeight=10.0
  var alignmentWeight= 10.0

  val seperationActivationDistance=10

  def setSeperation(value:Double) = seperationWeight=value
  def setCoherence(value:Double) = coherenceWeight= value
  def setAlignment(value:Double) = alignmentWeight=value
  def setFov(fovValue:Double) = fov=fovValue
  def setSpeed(b:Double)= speed=b

  var visibleBoids : Array[Boid] = Array[Boid]()


  def updateVisibleBoids =    // this is a temporary solution, should be reworked at some point
    var tmp=Array[Boid]()
    for each <- World.listOfBoids.filter(a=>a!=this) do
      if pos.distanceTo(each.pos) < fov then
        tmp=tmp.appended(each)
      visibleBoids=tmp


  // returns a unit vector for seperation, new value for speed, unit vector for cohesion

  def getMovementVectors: (Point,Double,Point) =
    var amount = visibleBoids.length
    var x = 0.0
    var y = 0.0

    var speedSum=0.0
    var mid:Point=pos

    for each <- visibleBoids do
      x += each.pos.x
      y += each.pos.y
      speedSum+=each.speed
      if pos.distanceTo(each.pos) < seperationActivationDistance  then
        mid=mid.-(mid.unitVectorTowards(each.pos))
    val aversion=pos.unitVectorTowards(mid./(visibleBoids.length))
    val speed=speedSum/(visibleBoids.length)
    val cohesion =  pos.unitVectorTowards(Point(x / amount, y / amount))
    (aversion,speed,cohesion)


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
      setSpeed(newSpeed)
      acceleration=acceleration.+(seperationVector.*(seperationWeight))
      acceleration=acceleration.+(cohesionVector.*(coherenceWeight))
    else
      acceleration=acceleration+speed

//having acceleration be a point makes some calculations easier

  // Changes the boids location to a new one based on the acceleration vector and its speed
  def move() =
    updateVisibleBoids
    applyMovementRules()
    val unitVectorScaled=pos.unitVectorTowards(acceleration).*(speed)
    pos=pos.+(unitVectorScaled)
    moveAcrossFrame()
    if pos.distanceTo(acceleration)>100 then acceleration=pos.+(pos.unitVectorTowards(acceleration).*(100))

// should everything be rendered at the same time? if boids are affected as they go then the changes will affect the movement of others
  override def toString()=
    s"Location: (${pos.x.round},${pos.y.round}) acceleration: (${acceleration.x.round},${acceleration.y.round}) length: ${pos.distanceTo(acceleration)}  speed: ${this.speed}"
}

// todo: boid eating?, evolution?, reproduction?, seeking food?