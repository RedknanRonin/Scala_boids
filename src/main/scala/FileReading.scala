package main

// class for handling saving and loading.
class FileReading(world:World){
  // loads a world with given inputs
  def loadWorld(boids: Array[Boid], predators: Array[Predator], foods: Array[Food], simulationEnabled: Boolean, mutationChanceVal: Double, SepSliderVal: Double,
                cohesionVal: Double, fovVal: Int, foodSpawnVal: Int, alignmentValue:Double) =
    world.emptyLists
    for each <- boids do world.spawnBoid(each)
    for each <- predators do world.spawnPredator(each)
    for each <- foods do world.spawnFood(each)
    world.simulationWorldEnabled = simulationEnabled
    world.mutationChance = mutationChanceVal
    world.seperationSliderState = SepSliderVal
    world.cohesionSliderState = cohesionVal
    world.fovSliderState = fovVal
    world.foodSpawnInterval = foodSpawnVal
    world.alignmentSliderState= alignmentValue


  // functions to parse strings into Boids, Predators and Foods
  def stringToBoid(input: String) =
    val variablesAsArray = input.split(",")
    Boid(Point(variablesAsArray(0).toDouble, variablesAsArray(1).toDouble), Point(variablesAsArray(2).toDouble, variablesAsArray(3).toDouble), world, variablesAsArray(4).toDouble.toInt, variablesAsArray(5).toDouble, variablesAsArray(6).toDouble, variablesAsArray(7).toDouble, variablesAsArray(8).toDouble,variablesAsArray(9).toDouble)

  def stringToPredator(input: String) =
    val variablesAsArray = input.split(",")
    Predator(Point(variablesAsArray(0).toDouble, variablesAsArray(1).toDouble), Point(variablesAsArray(2).toDouble, variablesAsArray(3).toDouble), world, variablesAsArray(4).toDouble.toInt, variablesAsArray(5).toDouble, variablesAsArray(6).toDouble)

  def stringToFood(input: String) =
    val variablesAsArray = input.split(",")
    Food(Point(variablesAsArray(0).toDouble, variablesAsArray(1).toDouble), world)

  // deciphers world loaded as string and loads it
  def decipherStringAndLoad(inpu: String) =
    try {
      var tbr = ""
      val pieces = inpu.split("END")
      if pieces.length == 10 then
        val boids: Array[Boid] = if pieces(0).nonEmpty then for each <- (pieces(0).split(";")) yield stringToBoid(each) else Array[Boid]()
        val predators: Array[Predator] = if pieces(1).nonEmpty then for each <- pieces(1).split(";") yield stringToPredator(each) else Array[Predator]()
        val foods: Array[Food] = if pieces(2) != "" then for each <- pieces(2).split(";") yield stringToFood(each) else Array[Food]()
        val simulationBool: Boolean = pieces(3).toBoolean
        val mutationCh: Double = pieces(4).toDouble
        val sepSliderState: Double = pieces(5).toDouble
        val cohSliderState: Double = pieces(6).toDouble
        val fovVal: Int = pieces(7).toInt
        val foodInterval: Int = pieces(8).toInt
        val alignmentValue=pieces(9).toDouble
        loadWorld(boids, predators, foods, simulationBool, mutationCh, sepSliderState, cohSliderState, fovVal, foodInterval,alignmentValue )
        tbr = ("Loading succesful")
      else
        tbr = ("Loading failed: Corrupted file")
      tbr
    }
    catch
      case _ => "Loading failed: Corrupted file"

  def load(input:String)=decipherStringAndLoad(input)
  def save=world.toString

}