package main

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

class world {
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

  var seperationSliderState=5
  var cohesionSliderState=0.25
  var fovSliderState=69
  
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
    stringToSave += (fovSliderState)
    stringToSave += "END"
    stringToSave += (foodSpawnInterval)
    stringToSave += "END"
    stringToSave

    // loads a world with given inputs
  def loadWorld(boids:Array[Boid],predators:Array[Predator],foods:Array[Food],simulationEnabled:Boolean,mutationChanceVal:Double,SepSliderVal:Int,
                cohesionVal:Int,fovVal:Int,foodSpawnVal:Int) =
    emptyLists
    for each<- boids do spawnBoid(each)
    for each <- predators do spawnPredator(each)
    for each <- foods do spawnFood(each)
    simulationWorldEnabled=simulationEnabled
    mutationChance=mutationChanceVal
    seperationSliderState=SepSliderVal
    cohesionSliderState=cohesionVal
    fovSliderState=fovVal
    foodSpawnInterval=foodSpawnVal
  

  // functions to parse strings into Boids, Predators and Foods
  def stringToBoid(input:String)=
    val variablesAsArray=input.split(",")
    Boid(Point(variablesAsArray(0).toDouble,variablesAsArray(1).toDouble),Point(variablesAsArray(2).toDouble,variablesAsArray(3).toDouble),this,variablesAsArray(4).toDouble.toInt,variablesAsArray(5).toDouble,variablesAsArray(6).toDouble,variablesAsArray(7).toDouble,variablesAsArray(8).toDouble)
  
  def stringToPredator(input:String)=
    val variablesAsArray = input.split(",")
    Predator(Point(variablesAsArray(0).toDouble, variablesAsArray(1).toDouble), Point(variablesAsArray(2).toDouble, variablesAsArray(3).toDouble), this, variablesAsArray(4).toDouble.toInt, variablesAsArray(5).toDouble, variablesAsArray(6).toDouble)

  def stringToFood(input:String)=
    val variablesAsArray = input.split(",")
    Food(Point(variablesAsArray(0).toDouble,variablesAsArray(1).toDouble),this)

  // deciphers world loaded as string and loads it
  def decipherStringAndLoad(inpu:String)=
    var tbr=""
    val pieces=inpu.split("END")
    if pieces.length==9 then
      val boids: Array[Boid] = if pieces(0).nonEmpty then for each <- (pieces(0).split(";")) yield stringToBoid(each) else Array[Boid]()
      val predators: Array[Predator] = if pieces(1).nonEmpty then for each <- pieces(1).split(";") yield stringToPredator(each) else Array[Predator]()
      val foods: Array[Food] = if pieces(2) != "" then for each <- pieces(2).split(";") yield stringToFood(each) else Array[Food]()
      val simulationBool: Boolean = pieces(3).toBoolean
      val mutationCh: Double = pieces(4).toDouble
      val sepSliderState: Int = pieces(5).toInt
      val cohSliderState: Int = pieces(6).toInt
      val fovVal: Int = pieces(7).toInt
      val foodInterval: Int = pieces(8).toInt
      loadWorld(boids,predators,foods,simulationBool,mutationCh,sepSliderState,cohSliderState,fovVal,foodInterval)
      tbr=("Loading worked")
    else
      tbr=("Loading failed: Corrupted file")
    tbr
}
