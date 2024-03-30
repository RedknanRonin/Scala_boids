package main

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

class world {
  val windowHeight= 640
  val windowWidth= 740
  var mutationChance:Double=0
  val seed= Random(69420)
  var listOfBoids=ArrayBuffer[Boid]()
  var listOfFoods=ArrayBuffer[Food]()
  
  var simulationWorldEnabled=true

  var foodTimer=0
  var foodSpawnInterval = 150

  def toggleSimulation=simulationWorldEnabled= !simulationWorldEnabled
  def setMutationChance(v:Double) = mutationChance=v
  def spawnBoid (b:Boid) = listOfBoids=listOfBoids.appended(b)
  def getBoid(number:Int) =  listOfBoids(number)
  def printBoids = for i <- listOfBoids.indices do println(i +" "+listOfBoids(i))
  def amountOfBoids = listOfBoids.length
  def setFoodInterval(withVal:Int) = foodSpawnInterval=withVal

  def getBoidAt(at:Point) =
    val boidsWithPos:Map[Point,Boid]=listOfBoids.map(a=> a.pos -> a).toMap  // returns a boid at a given point
    boidsWithPos.lift(at).get

  def deleteBoid(b:Boid) = listOfBoids-=b
  def deleteFood(food: Food) = listOfFoods-=food

  def tick=
    for each<- listOfBoids do each.move()

  def updateFoodTimer=
    if foodTimer == foodSpawnInterval then
      spawnFood(Food(Point(seed.between(0, windowWidth), seed.between(0, windowHeight)),this))
      foodTimer = 0
    else foodTimer += 1

  def spawnFood(f:Food) =
    listOfFoods= listOfFoods.appended(f)

}
