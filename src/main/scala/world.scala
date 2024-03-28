package main

import scala.util.Random

class world {
  val windowHeight= 640
  val windowWidth= 740
  var mutationChance:Double=0
  val seed= Random(69420)
  var listOfBoids=Array[Boid]()


  def setMutationChance(v:Double) = mutationChance=v
  def spawnBoid (b:Boid) = listOfBoids=listOfBoids.appended(b)
  def getBoid(number:Int) =  listOfBoids(number)
  def printBoids = for i <- listOfBoids.indices do println(i +" "+listOfBoids(i))

  def amountOfBoids = listOfBoids.length

  def getBoidAt(at:Point) =
    val boidsWithPos:Map[Point,Boid]=listOfBoids.map(a=> a.pos -> a).toMap  // returns a boid at a given point
    boidsWithPos.lift(at).get

  def deleteBoid(at:Point) = listOfBoids.filter(a => a!=getBoidAt(at))

  def tick= for each<- listOfBoids do each.move()

}
