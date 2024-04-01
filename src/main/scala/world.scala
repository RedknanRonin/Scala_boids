package main

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

class world {
  val windowHeight= 640
  val windowWidth= 740
  var mutationChance:Double=0.3
  val seed= Random(69420)
  var listOfBoids=ArrayBuffer[Boid]()
  var listOfFoods=ArrayBuffer[Food]()
  var listOfPredators=ArrayBuffer[Predator]()

  def emptyLists=
    listOfFoods=listOfFoods.empty
    listOfBoids=listOfBoids.empty
    listOfPredators=listOfPredators.empty

  var seperationSliderState=30
  var cohesionSliderState=20
  var fovSliderState=100
  
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
  def deletePredator(p:Predator) =listOfPredators-=p
  def spawnPredator(p:Predator) = listOfPredators+=p

  def tick=
    for each<- listOfBoids do each.move()

  def updateFoodTimer=
    if foodTimer == foodSpawnInterval then
      spawnFood(Food(Point(seed.between(0, windowWidth), seed.between(0, windowHeight)),this))
      foodTimer = 0
    else foodTimer += 1

  def spawnFood(f:Food) =
    listOfFoods= listOfFoods.appended(f)

  //todo: file format, raw string??
  def worldAsString(): String =
    var stringToSave: String = ""
    for each <- listOfBoids do stringToSave += s"$each;"
    stringToSave += "END"
    for each <- listOfPredators do stringToSave += s"$each;" // these can be empty
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
    //todo: save into txt file
    //boids seperated by ;, predators seperated by ;, foods seperated by ; , simulation enabled, mutation chance, seperation slider value, cohesion slider value, fov slider value, foodspawninterval

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

    //Todo break txt file into pieces and pass as var

  def stringToBoid(input:String)=
    val variablesAsArray=input.split(",")
    Boid(Point(variablesAsArray(0).toDouble,variablesAsArray(1).toDouble),Point(variablesAsArray(2).toDouble,variablesAsArray(3).toDouble),this,variablesAsArray(4).toDouble.toInt,variablesAsArray(5).toDouble,variablesAsArray(6).toDouble,variablesAsArray(7).toDouble,variablesAsArray(8).toDouble)
  
  def stringToPredator(input:String)=
    val variablesAsArray = input.split(",")
    Predator(Point(variablesAsArray(0).toDouble, variablesAsArray(1).toDouble), Point(variablesAsArray(2).toDouble, variablesAsArray(3).toDouble), this, variablesAsArray(4).toDouble.toInt, variablesAsArray(5).toDouble, variablesAsArray(6).toDouble)

  def stringToFood(input:String)=
    val variablesAsArray = input.split(",")
    Food(Point(variablesAsArray(0).toDouble,variablesAsArray(1).toDouble),this)


  def decipherStringAndLoad(inpu:String)=
    val pieces=inpu.split("END")
    println(inpu.split("END").mkString("        "))
    val boids= if pieces(0).nonEmpty then for each <-(pieces(0).split(";")) yield stringToBoid(each) else Array[Boid]()
    val predators=if pieces(1).nonEmpty then for each <-pieces(1).split(";") yield stringToPredator(each) else Array[Predator]()
    val foods= if pieces(2)!="" then for each <- pieces(2).split(";") yield  stringToFood(each) else Array[Food]()
    val simulationBool=pieces(3).toBoolean
    val mutationCh=pieces(4).toDouble
    val sepSliderState=pieces(5).toInt
    val cohSliderState=pieces(6).toInt
    val fovVal=pieces(7).toInt
    val foodInterval=pieces(8).toInt
    loadWorld(boids,predators,foods,simulationBool,mutationCh,sepSliderState,cohSliderState,fovVal,foodInterval)
}
