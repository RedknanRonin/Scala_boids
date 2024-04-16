package main
import main.boidsGUI.start
import scalafx.animation.AnimationTimer
import scalafx.application.JFXApp3
import scalafx.geometry.{Insets, Orientation, Pos}
import scalafx.scene.Scene
import scalafx.scene.canvas.Canvas
import scalafx.scene.control.{Button, ColorPicker, Label, Menu, MenuBar, MenuItem, RadioButton, Separator, Slider, Spinner, ToggleButton, ToggleGroup}
import scalafx.scene.layout.{BorderPane, HBox, VBox}
import scalafx.scene.paint.Color
import scalafx.scene.text.{Font, FontWeight}
import scalafx.stage.FileChooser
import scalafx.stage.FileChooser.ExtensionFilter

import java.io.{File, FileWriter}
import java.util.ConcurrentModificationException
import scala.io.Source



// The program can be ran by running the boidsGUI object
object boidsGUI extends JFXApp3:
  val WORLD = World()
  val canvas = Canvas(WORLD.windowWidth, WORLD.windowHeight)

  val gc = canvas.graphicsContext2D
  val randomSeed = WORLD.seed
  var drawFovLines: Boolean = false
  var paused = true
  var allowFoodSpawnsAndSimulation = WORLD.simulationWorldEnabled


  // draws a given boid at its pos, boids are triangles and can vary in color.
  def drawBoid(boid:Boid) =
      val (at,dest,fov) = (boid.pos,boid.velocity,boid.viewRange)
      val unitVtoDest=at.unitVectorTowards(dest)

      val top=at.+(unitVtoDest.*(20))
      val btLeft=at.+(unitVtoDest.perpendicular.*(5))
      val btRight=at.-(unitVtoDest.perpendicular.*(5))

      val (leftEnd,rightEnd) = boid.calculateFOVEndpoints

      gc.fill = boid.getColour
      gc.fillPolygon(Array((top.x,top.y),(btRight.x,btRight.y),(btLeft.x,btLeft.y)))

      if drawFovLines then
        gc.strokeLine(at.x,at.y,rightEnd.x,rightEnd.y)
        gc.strokeLine(at.x, at.y, leftEnd.x, leftEnd.y)

  val statisticsPanel = Canvas(150, 100)


  // updates statistics panel to reflect current boid to predator ratio
  // if simulation mode is turned off, displays only the amount of boids.
  def updateStatistics =
    statisticsPanel.graphicsContext2D.clearRect(0, 0, 150, 170)
    def totalAmount = (WORLD.listOfBoids.length + WORLD.listOfPredators.length).toFloat
    if totalAmount!=0 then
      val boidLength = (WORLD.listOfBoids.length.toFloat / totalAmount)
      val shrimpLength = 1-boidLength
        if WORLD.simulationWorldEnabled then
          statisticsPanel.graphicsContext2D.fill = Color.Blue
          statisticsPanel.graphicsContext2D.fillRect(0, 20, 150, 20)
          statisticsPanel.graphicsContext2D.fill = Color.Red
          statisticsPanel.graphicsContext2D.fillRect(150*boidLength, 20, 150*shrimpLength, 20)

          statisticsPanel.graphicsContext2D.fillText(s"Boids: ${WORLD.listOfBoids.length}",20,60)
          statisticsPanel.graphicsContext2D.fillText(s"Predators: ${WORLD.listOfPredators.length}", 20, 80)
      else
        statisticsPanel.graphicsContext2D.fillText(s"Boids: ${WORLD.listOfBoids.length}", 20, 60)

  // function used to update position of boids and draw them
  var listOfFoodsForCurrentTick=WORLD.listOfFoods
  var listOfBoidsCurrentTick=WORLD.listOfBoids
  var predatorsCurrentTick=WORLD.listOfPredators


  def tick(move:Boolean) =  // Some ticks don't want to move anything, thus the flag
    listOfFoodsForCurrentTick = WORLD.listOfFoods
    listOfBoidsCurrentTick = WORLD.listOfBoids
    predatorsCurrentTick = WORLD.listOfPredators
    gc.clearRect(0, 0, canvas.width.value, canvas.height.value)
    gc.fill = Color.Gray
    gc.fillRect(0, 0, canvas.width.value, canvas.height.value)

      // Raises ConcurrentModificationException due to appending lists while iterating them
      // However this seemingly doesn't affect the program much so errors are ignored.
    try {
    for each <- listOfBoidsCurrentTick do
        if move then each.move()
        drawBoid(each)
    if WORLD.simulationWorldEnabled then
      WORLD.updateFoodTimer
      for each <- listOfFoodsForCurrentTick do
        drawFood(each)
      for each <- predatorsCurrentTick do
        if move then each.move()
        drawPredator(each) }
    catch
      case _=>   // ignores errors, missing one tick doesn't cause huge issues
          
    updateStatistics


  def spawnBoid(boid:Boid) =
    WORLD.spawnBoid(boid)
    drawBoid(boid)

  def drawFood(food:Food)=
    val pos=food.pos
    gc.fill=Color.Green
    gc.fillOval(pos.x, pos.y, 10, 10)

  //draws a given predator. Predators are red with a dot in their noses making them resemble shrimps.
  def drawPredator(p:Predator) =
    val (at, dest, fov) = (p.pPos, p.pVelocity, p.viewRange)
    val unitVtoDest = at.unitVectorTowards(dest)
    val top = at.+(unitVtoDest.*(20))
    val btLeft = at.+(unitVtoDest.perpendicular.*(5))
    val btRight = at.-(unitVtoDest.perpendicular.*(5))
    val (leftEnd, rightEnd) = p.calculateFOVEndpoints
    gc.fill = Color.Red
    gc.fillPolygon(Array((top.x, top.y), (btRight.x, btRight.y), (btLeft.x, btLeft.y)))
    gc.fill = Color.Black
    gc.fillOval(top.x-1.5, top.y-1.5,3,3)

    if drawFovLines then
      gc.strokeLine(at.x, at.y, rightEnd.x, rightEnd.y)
      gc.strokeLine(at.x, at.y, leftEnd.x, leftEnd.y)



  // block used to handle animations and running the program
  val timer = AnimationTimer(time => {
      tick(true)   // at high capacity causes some stuttering
  })

  def pause()= paused = !paused



  def start() =
    stage = new JFXApp3.PrimaryStage:
      title = "Boids"
      width = 1100 //values loosely based on golden ratio
      height = 680
      resizable = false
      gc.fill = Color.Gray
      gc.fillRect(0,0,WORLD.windowWidth,WORLD.windowHeight)

    //logs are empty at first and are filled up
    val firstLog = new Label(""):
      font = Font("System", FontWeight.ExtraBold, 14)
    val secondLog = new Label("")
    val thirdLog = new Label("")


    def updateLog(text:String)=
      thirdLog.text=secondLog.text.value
      secondLog.text = firstLog.text.value
      firstLog.text = text

    val logPanel = new VBox:
      margin = Insets(10)
      children = Array(Separator(Orientation.Horizontal),firstLog, secondLog,thirdLog)


    val pauseButton= new ToggleButton("Run")

    def selectFileSave() =
      val fileChooserForSave = FileChooser()
      fileChooserForSave.extensionFilters.add(ExtensionFilter("txt", "*.txt"))
      fileChooserForSave.initialDirectory = new File(".")
      val fileToSaveTo=fileChooserForSave.showSaveDialog(stage)
      if fileToSaveTo != null then
        updateLog(s"Saved to: ${fileToSaveTo.getName}")
        val fileWriter = new FileWriter(fileToSaveTo)
        val worldAsString: String = FileReading(WORLD).save
        fileWriter.write(worldAsString)
        fileWriter.close()

      else
        updateLog("No file selected")
        null

    val saveButton = new Button("Save")
    saveButton.onMouseReleased = (event) =>
      updateLog("Saving")
      selectFileSave()

    val clearButton = new Button("Clear screen"):
      this.onMouseReleased = (event) =>
        updateLog("Cleared screen")
        WORLD.emptyLists
        tick(false)

      //moves sliders to correct values after loading



    def selectFile() =
      val fileChooserForLoad = FileChooser()
      fileChooserForLoad.extensionFilters.add(ExtensionFilter("txt","*.txt"))
      fileChooserForLoad.initialDirectory = new File(".")
      // FileChooser returns a file, or null if the user closed the window without selecting one.
      val selectedFile = fileChooserForLoad.showOpenDialog(stage)
      if selectedFile != null then
        updateLog(s"Loading from: ${selectedFile.getName}")
        selectedFile
      else
        updateLog("No file selected")
        null



    val spawnBoids= new Button("Spawn boid")



    val fovToggler = new ToggleButton("Show fov")
    fovToggler.onMouseReleased = (event) =>
      updateLog("Toggled fov")
      drawFovLines= !drawFovLines
      tick(false)

    val spawnPredatorButton = new Button("Spawn predator")
    spawnPredatorButton.onMouseReleased = (event)=>
      updateLog("Spawned predator")
      val at = Point(randomSeed.nextDouble() * 750, randomSeed.nextDouble() * 750)
      val dest = Point(randomSeed.nextDouble() * 750, randomSeed.nextDouble() * 750)
      val predator=Predator(at,dest,WORLD,10,8,150)
      WORLD.spawnPredator(predator)
      if WORLD.simulationWorldEnabled then drawPredator(predator)


    val simulationModeButton = new ToggleButton("Simulation mode")
    simulationModeButton.onMouseReleased = (event)=>
      updateLog("Toggled mode")
      WORLD.toggleSimulation
      simulationModeButton.text = if WORLD.simulationWorldEnabled then ("Boid mode") else ("Simulation mode")


    val togglers = new HBox(10):
      children = Array(fovToggler,spawnPredatorButton,simulationModeButton)
      margin = Insets(5,5,5,5)



    val seperationSlider = new Slider(0,2,WORLD.seperationSliderState):
      this.autosize()

    val alignmentSlider= new Slider(0,2, WORLD.alignmentSliderState)

    val avoidanceSliderInBox= new VBox(Label("Avoidance"),seperationSlider)

    val coherenceSlider = new Slider(0,2,WORLD.cohesionSliderState):
      this.autosize()
    val coherenceSliderInBox= new VBox(Label("Coherence"),coherenceSlider)

    def updateSliders =
      seperationSlider.value = WORLD.seperationSliderState
      coherenceSlider.value = WORLD.cohesionSliderState
      alignmentSlider.value = WORLD.alignmentSliderState

    val loadButton = new Button("Load")
    loadButton.onMouseReleased = (event) =>
      try {
        val fileToloadFrom: File = selectFile()
        val source = Source.fromFile(fileToloadFrom)
        val string = source.mkString("")
        source.close()
        updateLog(FileReading(WORLD).load(string))
        tick(false) // draws everything
        updateSliders
      }
      catch
        case _ => updateLog("Loading failed")

    val buttons = new VBox(20):
      children = Array(pauseButton, saveButton, loadButton, spawnBoids)
      margin = Insets(5, 5, 5, 20)

    val mutationChanceSlider = new Slider(0,1,WORLD.mutationChance):
      this.onMouseReleased =  (event) =>
        WORLD.setMutationChance(this.value.get())
      this.autosize()

    var mutationChance = mutationChanceSlider.value.get()
    val mutationChanceLabel = new Label("Mutation chance: " + mutationChance)
    val mutationBox = VBox(mutationChanceLabel, mutationChanceSlider)

    val fovSlider= new Slider(1,360,WORLD.fovSliderState):
      this.autosize()

    var fov=fovSlider.value.get().round.toString
    val fovLabel= new Label("FOV: "+fovSlider.value.get().toInt)
    val fovBox= new VBox(fovLabel,fovSlider)



    val foodSpawnrateSlider = new Slider(0,250,WORLD.foodSpawnInterval):
      this.autosize()
    var foodSpawnrate=250-foodSpawnrateSlider.value.get().toInt
    val foodSpawnrateLabel= new Label("Food spawnrate")
    val foodSpawnrateBox = new VBox(foodSpawnrateLabel,foodSpawnrateSlider)


    val sliders = new VBox(10):
      children=Array(avoidanceSliderInBox, coherenceSliderInBox, VBox(Label("Alignment"),alignmentSlider), mutationBox,foodSpawnrateBox,fovBox)

    //sidepanel for settings and statistics
    val settingPane= new VBox(20):
      prefHeight = 700
      prefWidth  = 300
      margin = Insets(10,40,10,10)
      children=Array(HBox(VBox(statisticsPanel,clearButton),buttons),togglers,sliders ,logPanel)

    //canvas where boids are drawn
    val boidWorld = new HBox:
      children = Array(canvas,Separator(Orientation.Vertical))

    val root = BorderPane(boidWorld, null, settingPane,null , null)
    stage.scene = Scene(parent = root)


    pauseButton.onMouseReleased = (event) =>
      pause()
      if paused then
        timer.stop()
        pauseButton.text="Run"
        updateLog("Paused")
      else
        pauseButton.text="Pause"
        updateLog("Unpaused")
        timer.start()


    mutationChanceSlider.onMouseReleased  = (event) =>
      mutationChance=mutationChanceSlider.value.get()
      mutationChanceLabel.text = ("Mutation chance: "+mutationChance.toString.take(4))
      updateLog("Changed mutation chance")

    alignmentSlider.onMouseReleased = (event)=>
        updateLog("Changesd alignment value")
        WORLD.alignmentSliderState=alignmentSlider.value.get()
        for each <- WORLD.listOfBoids do each.setAlignment(alignmentSlider.value.get())
        tick(false)

    foodSpawnrateSlider.onMouseReleased = (event) =>
      updateLog("Changed food spawnrate")
      WORLD.setFoodInterval(250-foodSpawnrateSlider.value.get().toInt)
      foodSpawnrateLabel.text =("Food spawnrate")
      WORLD.foodTimer=0


    fovSlider.onMouseReleased =  (event) =>
      updateLog("Changed fov")
      fov=fovSlider.value.get().round.toString
      fovLabel.text = ("FOV: "+fov)
      tick(false) // updates when paused
      for each <- WORLD.listOfBoids do each.setFov(fovSlider.value.get())


    coherenceSlider.onMouseReleased = (event) =>
      tick(false) // updates when paused
      for each <- WORLD.listOfBoids do each.setCoherence(coherenceSlider.value.get())
      updateLog("Changed coherence")

    seperationSlider.onMouseReleased = (event) =>
      tick(false) // updates when paused
      updateLog("Changed avoidance")
      for each <- WORLD.listOfBoids do each.setSeperation(seperationSlider.value.get())

    //spawns boid at random location with random direction. Values for Sep, Coh and Fov are from sliders' current positions.
    spawnBoids.onMouseReleased = (event) =>
      updateLog("Spawned boid")
      val point=Point(randomSeed.nextDouble()*750,randomSeed.nextDouble()*750)
      val dest= Point(randomSeed.nextDouble()*750,randomSeed.nextDouble()*750)
      val aBoid=Boid(point,dest,WORLD,seperationSlider.value.get(),coherenceSlider.value.get(),fovSlider.value.get())
      spawnBoid(aBoid)

  end start

end boidsGUI


