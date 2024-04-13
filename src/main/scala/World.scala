package main

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

class World {
  val windowHeight= 640
  val windowWidth= 740
  var mutationChance:Double=0.3
  val seed= Random(69420) // or preferrably System.nanoTime()
  var listOfBoids=ArrayBuffer[Boid]()
  var listOfFoods=ArrayBuffer[Food]()
  var listOfPredators=ArrayBuffer[Predator]()

  // empties lists to clear screen of everything
  def emptyLists=
    listOfFoods=listOfFoods.empty
    listOfBoids=listOfBoids.empty
    listOfPredators=listOfPredators.empty

  var seperationSliderState:Double=5
  var cohesionSliderState: Double = 0.25
  var fovSliderState: Int = 69
  
  var simulationWorldEnabled=true

  //timers for food spawning
  var foodTimer=0
  var foodSpawnInterval = 150

  def toggleSimulation=simulationWorldEnabled= !simulationWorldEnabled
  def setMutationChance(v:Double) = mutationChance=v
  def spawnBoid (b:Boid) = listOfBoids=listOfBoids.appended(b)
  def printBoids = for i <- listOfBoids.indices do println(i +" "+listOfBoids(i))
  def amountOfBoids = listOfBoids.length
  def setFoodInterval(withVal:Int) = foodSpawnInterval=withVal


  def deleteBoid(b:Boid) = listOfBoids-=b
  def deleteFood(food: Food) = listOfFoods-=food
  def deletePredator(p:Predator) =listOfPredators-=p
  def spawnPredator(p:Predator) = listOfPredators+=p

  def tickFromWorld=
    for each<- listOfBoids do each.move()

  def updateFoodTimer=
    if foodTimer == foodSpawnInterval then
      spawnFood(Food(Point(seed.between(0, windowWidth), seed.between(0, windowHeight)),this))
      foodTimer = 0
    else foodTimer += 1

  def spawnFood(f:Food) =
    listOfFoods= listOfFoods.appended(f)

  // a representation of the current world in a string format
  // the format is:  boids seperated by ;, predators seperated by ;, foods seperated by ; , simulation enabled, mutation chance, seperation slider value, cohesion slider value, fov slider value, foodspawninterval
  def worldAsString(): String =
    var stringToSave: String = ""
    for each <- listOfBoids do stringToSave += s"$each;"
    stringToSave += "END"
    for each <- listOfPredators do stringToSave += s"$each;"
    stringToSave += "END"
    for each <- listOfFoods do stringToSave+=s"$each;"
    stringToSave += "END"
    stringToSave += simulationWorldEnabled
    stringToSave += "END"
    stringToSave += (mutationChance)
    stringToSave += "END"
    stringToSave += (seperationSliderState)
    stringToSave += "END"
    stringToSave += (cohesionSliderState)
    stringToSave += "END"
    stringToSave += (fovSliderState.toInt)
    stringToSave += "END"
    stringToSave += (foodSpawnInterval)
    stringToSave
}
