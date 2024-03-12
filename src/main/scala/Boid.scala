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

  def unitVectorTowards(other:Point) = other.-(this)./(this.distanceTo(other))

}

class Boid(var pos:Point, var acceleration:Point) {
  var speed:Double=5
  var fov:(Double)= 100.0
  var seperationWeight=30.0
  var coherenceWeight=1.0
  var alignmentWeight= 1.0


  val seperationActivationDistance=10

  def setSeperation(value:Double) = seperationWeight=value
  def setCoherence(value:Double) = coherenceWeight= value
  def setAlignment(value:Double) = alignmentWeight=value
  def setFov(fovValue:Double) = fov=fovValue
  def setSpeed(b:Double)= speed=b

  var visibleBoids : Array[Boid] = Array[Boid]()

  def updateVisibleBoids =    // this is a temporary solution, should be reworked at some point
    var tmp=Array[Boid]()
    for each <- world().listOfBoids.filter(a=>a!=this) do
      if pos.distanceTo(each.pos) < fov then
        tmp:+=(each)
    this.visibleBoids=tmp


  def seperation: (Point,Double) =             // returns a unit vector that pushes boid away from others and the average speed of other boids
    var speedSum=0.0
    var mid:Point=pos
    for each <- visibleBoids do
      speedSum+=each.speed
      if pos.distanceTo(each.pos)< seperationActivationDistance  then
        mid=mid.-(mid.unitVectorTowards(each.pos))
    (pos.unitVectorTowards(mid./(visibleBoids.length)),speedSum/(visibleBoids.length+1))

  // returns a unit vector towards the average location of the boids visible to current boid.
  def cohesion: Point =
    var amount = visibleBoids.length
    var x = 0.0
    var y = 0.0
    for each <- visibleBoids do
      x += each.pos.x
      y += each.pos.y
    pos.unitVectorTowards(Point(x / amount, y / amount))

// moves the boid across the edges of the frame, also shifts velocity vector accordingly
  def moveAcrossFrame() =
    val maxX=world().windowWidth
    val maxY=world().windowHeight

    if (pos.x > maxX) then pos=Point(pos.x-maxX,pos.y)
    if acceleration.x > maxX then acceleration=Point(acceleration.x-maxX,acceleration.y)
    
    if (pos.x < 0) then pos=Point(pos.x+maxX,pos.y)
    if acceleration.x < 0 then acceleration=Point(acceleration.x+maxX,acceleration.y)
    
    if (pos.y > maxY) then pos=Point(pos.x,pos.y-maxY)
    if acceleration.y > maxY then acceleration=Point(acceleration.x,acceleration.y-maxY)
    
    if (pos.y < 0) then pos=Point(pos.x,pos.y + maxY)
    if acceleration.y < 0 then acceleration=Point(acceleration.x,acceleration.y+maxY)

  def applyMovementRules() =
    if visibleBoids.length!=0 then
      val (seperationVector,newSpeed)= seperation
      val cohesionVector=cohesion
      setSpeed(newSpeed)
      acceleration=acceleration.+(seperationVector.*(seperationWeight))
      acceleration=acceleration.+(cohesionVector.*(coherenceWeight))
    else
      acceleration=acceleration.*(1.1)

//having acceleration be a point makes some calculations easier


  // Changes the boids location to a new one based on the acceleration vector and its speed
  def move() =
    updateVisibleBoids
    applyMovementRules()
    val unitVector=pos.unitVectorTowards(acceleration)
    val scaled=unitVector.*(speed)
    pos=pos.+(scaled)
    moveAcrossFrame()

// should everything be rendered at the same time? if boids are affected as they go then the changes will affect the movement of others
  override def toString()=
    s"Location: (${pos.x.round},${pos.y.round}) acceleration: (${acceleration.x.round},${acceleration.y.round}) length: ${pos.distanceTo(acceleration)}  speed: ${this.speed}"
}
  // animation timer
@main def test() =
  val testBoid=Boid(Point(100,100),Point(110,110))
  val otherBoid=Boid(Point(101,101),Point(110,110))
  val w=world()
  otherBoid.setSpeed(20)
  w.spawnBoid(testBoid)
  w.spawnBoid(otherBoid)
  w.printBoids
  w.tick()
  for each <- testBoid.visibleBoids do println("as "+each)
  w.printBoids

// todo: boid eating?, evolution?, reproduction?, seeking food?
// todo maybe still get basic boids working first