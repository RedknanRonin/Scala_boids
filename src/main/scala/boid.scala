package main
import scalafx.scene.Scene
import scalafx.scene.canvas.Canvas
import scalafx.scene.paint.Color
import scalafx.scene.shape.{Polygon, Shape}
import scalafx.stage.Stage

case class point(x:Double,y:Double){
  def +(otherPoint:point) = point(x+otherPoint.x,y+otherPoint.y)
  def -(otherPoint:point) = point(x-otherPoint.x,y-otherPoint.y)
  def /(number:Double) = point(x/number,y/number)
  def *(number:Double) = point(x*number,y*number)
  def distanceTo(other:point)= math.sqrt((this.x+other.x)*(this.x+other.x)+(this.y+other.y)*(this.y+other.y))
  def unitVectorTowards(other:point) = other.-(this)/(this.distanceTo(other))

}

class boid(var pos:point,var acceleration:point) {
  var speed=1
  var fov:(Double,Int)= (120.1,10)
  var seperationWeight=1.1
  var coherenceWeight=1.1
  var alignmentWeight= 1.1

  def setSeperation(value:Float) = seperationWeight=value
  def setCoherence(value:Float) = coherenceWeight= value
  def setAlignment(value:Float) = alignmentWeight=value
  def setFov(fovValue:Double,lengthValue:Int) = fov=(fovValue,lengthValue)

  def visibleBoids : Array[boid] = world().listOfBoids


  def seperation =             // returns a unit vector that is opposite of the other boid and average speed of other boids
    var speedSum=0
    var x=pos.x
    var y=pos.y
    for each <- visibleBoids do
      speedSum+=each.speed
      if pos.distanceTo(each.pos)>= 50 then
        x+=pos.x-each.pos.x
        y+=(pos.y-each.pos.y)
    (pos.unitVectorTowards( point(x,y)./(visibleBoids.length)),speedSum/visibleBoids.length)

   // returns a unit vector towards the average location of the boids visible to current boid.
  def cohesion =
    var amount=visibleBoids.length
    var x=0.0
    var y=0.0
    for each <- visibleBoids do
      x+=each.pos.x
      y+=each.pos.y
    pos.unitVectorTowards(point(x/amount,y/amount))

// moves the boid across the edges of the frame, also shifts velocity vector accordingly
  def moveAcrossFrame =
    val maxX=world().windowWidth
    val maxY=world().windowHeight

    if (pos.x > maxX) then
      pos=point(pos.x-maxX,pos.y)
      acceleration=point(acceleration.x-maxX,acceleration.y)
    if (pos.x < 0) then
      pos=point(pos.x+maxX,pos.y)
      acceleration=point(acceleration.x+maxX,acceleration.y)
    if (pos.y > maxY) then
      pos=point(pos.x,pos.y-maxY)
      acceleration=point(acceleration.x,acceleration.y-maxY)
    if (pos.y < 0) then
      pos=point(pos.x,pos.y + maxY)
      acceleration=point(acceleration.x,acceleration.y+maxY)

  def applyMovementRules =
    val sep= seperation
    val newSpeed=sep._2
    val seperationVector=sep._1
    val cohesionVector=cohesion
    speed=newSpeed
    acceleration.+(seperationVector.*(seperationWeight))
    acceleration.+(cohesionVector.*(coherenceWeight))






  // Changes the boids location to a new one based on the acceleration vector and its speed
  def move =
    val unitVector=pos.unitVectorTowards(acceleration)
    val scaled=(unitVector).*(speed)
    pos=pos.+(scaled)
    moveAcrossFrame

// should everything be rendered at the same time? if boids are affected as they go then the changes will affect the movement of others
  override def toString()=
    s"Location: (${pos.x},${pos.y}) velocity: (${acceleration.x},${acceleration.y}) length: ${pos.distanceTo(acceleration)} "
}

@main def test() =
  val testBoid=boid(point(100,100),point(20,120))
  testBoid.move
  println(testBoid)


