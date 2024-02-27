package main
import scalafx.scene.Scene
import scalafx.scene.canvas.Canvas
import scalafx.scene.paint.Color
import scalafx.scene.shape.{Polygon, Shape}
import scalafx.stage.Stage

case class point(x:Double,y:Double){
  def +(otherPoint:point) = point(x+otherPoint.x,y+otherPoint.y)
  def distanceTo(other:point)= math.sqrt((this.x+other.x)*(this.x+other.x)+(this.y+other.y)*(this.y+other.y))

}

class boid(var pos:point,var velocity:point) {
  val speed=5
  var fov:(Double,Int)= (120.1,10)
  var avoidanceWeight=1.1
  var coherenceWeight=1.1
  var alignmentWeight= 1.1

  def setAvoidance(value:Float) = avoidanceWeight=value
  def setCoherence(value:Float) = coherenceWeight= value
  def setAlignment(value:Float) = alignmentWeight=value
  def setFov(fovValue:Double,lengthValue:Int) = fov=(fovValue,lengthValue)


  def visibleBoids : Array[boid] = ???

  def distanceToBoid(other:boid) = pos.distanceTo(other.pos)

  def seperation =             // returns a point (vector) that is opposite of the other boid
    var amount=0.0
    var x=pos.x
    var y=pos.y
    for each <- visibleBoids do
      if pos.distanceTo(each.pos)>= 50 then
        x+=pos.x-each.pos.x
        y+=(pos.y-each.pos.y)
    point(x/amount,y/amount)

   // returns the average location of the boids visible to current boid. The vector is also the speed
  def cohesion =
    var amount=visibleBoids.length
    var x=0.0
    var y=0.0
    for each <- visibleBoids do
      x+=each.pos.x
      y+=each.pos.y
    point(x/amount,y/amount)

// should everything be rendered at the same time? if boids are affected as they go then the changes will affect the movement of others
  override def toString()=
    s"Location: (${pos.x},${pos.y}) velocity: (${velocity.x},${velocity.y}) length: ${pos.distanceTo(velocity)} "
}

@main def test() =
  println(boid(point(2,3),point(3,4)))
