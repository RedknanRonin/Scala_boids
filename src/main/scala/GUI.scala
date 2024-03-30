package main
import scalafx.animation.AnimationTimer
import scalafx.application.JFXApp3
import scalafx.geometry.{Insets, Orientation, Pos}
import scalafx.scene.Scene
import scalafx.scene.canvas.Canvas
import scalafx.scene.control.{Button, ColorPicker, Label, Menu, MenuBar, MenuItem, RadioButton, Separator, Slider, Spinner, ToggleButton, ToggleGroup}
import scalafx.scene.layout.{BorderPane, HBox, VBox}
import scalafx.scene.paint.Color
import scalafx.scene.text.{Font, FontWeight}

import scala.math.{abs, pow}


object boidsGUI extends JFXApp3:

  val WORLD = world()
  val canvas = Canvas(WORLD.windowWidth, WORLD.windowHeight)

  val gc = canvas.graphicsContext2D
  val randomSeed = WORLD.seed
  var drawViewLine: Boolean = false
  var paused = true
  var drawViewCircle = false
  var allowFoodSpawnsAndSimulation = WORLD.simulationWorldEnabled

  def drawBoid(boid:Boid) =
      val (at,dest,fov) = (boid.pos,boid.velocity,boid.fov)
      val unitVtoDest=at.unitVectorTowards(dest)
      val top=at.+(unitVtoDest.*(20))
      val btLeft=at.+(unitVtoDest.perpendicular.*(5))
      val btRight=at.-(unitVtoDest.perpendicular.*(5))

      gc.fill = boid.getColour
    //  gc.fillOval(at.x,at.y,10,10)
      gc.fillPolygon(Array((top.x,top.y),(btRight.x,btRight.y),(btLeft.x,btLeft.y)))

      if drawViewLine then gc.strokeLine(at.x,at.y,dest.x,dest.y)
      if drawViewCircle then gc.strokeOval(at.x-fov/2,at.y-fov/2,fov,fov)


  def tick =
    for each <- WORLD.listOfBoids do
      each.move()
      drawBoid(each)
    if WORLD.simulationWorldEnabled then
      WORLD.updateFoodTimer
      for each <- WORLD.listOfFoods do
        drawFood(each)

  def spawnBoid(boid:Boid) =
    WORLD.spawnBoid(boid)
    drawBoid(boid)

  def drawFood(food:Food)=   //todo: implement food
    val pos=food.pos
    gc.fill=Color.Red
    gc.fillOval(pos.x, pos.y, 10, 10)



  def printDebug =
    println("****************************")
    println("\nBoids: "+WORLD.listOfBoids.length)
    println("****************************")
    for eah<- WORLD.listOfBoids do println(eah)
    println("****************************")
    println("FOODS: "+WORLD.listOfFoods.length)
    for eac <- WORLD.listOfFoods do println(eac)



  var lastTime = System.nanoTime()
  val timer = AnimationTimer(time => {
    val deltaTime = (time - lastTime)
    lastTime = time
    gc.clearRect(0,0,canvas.width.value,canvas.height.value)
    gc.fill = Color.Gray
    gc.fillRect(0,0,canvas.width.value,canvas.height.value)
    tick   //tick should utilize deltaTime


  })

  def pause()=
    if paused then
      paused=false
    else paused=true


  def start() =
    stage = new JFXApp3.PrimaryStage:
      title = "Boids"
      width = 1100 //values loosely based on golden ratio
      height = 680
      resizable = false

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

    val saveButton = new Button("Save")
    saveButton.onMouseReleased = (event) =>
      updateLog("Saving")
      printDebug


    val loadButton = new Button("Load")
    loadButton.onMouseReleased = (event)=>
      updateLog("Load")
      tick

    val spawnBoids= new Button("Spawn boid")

    val buttons = new VBox(20):
      children=Array(pauseButton,saveButton,loadButton,spawnBoids)
      margin = Insets(5,5,5,20)


    val fovToggler = new Button("Show fov")
    fovToggler.onMouseReleased = (event) =>
      updateLog("Toggled fov")
      drawViewCircle= !drawViewCircle

    val lineToggler = new Button("Show direction")
    lineToggler.onMouseReleased = (event)=>
      updateLog("Toggled direction")
      drawViewLine= !drawViewLine

    val simulationModeButton = new Button("Free mode")
    simulationModeButton.onMouseReleased = (event)=>
      WORLD.toggleSimulation
      simulationModeButton.text = if WORLD.simulationWorldEnabled then ("Free mode") else ("Simulation mode")




    val togglers = new HBox(10):
      children = Array(fovToggler,lineToggler,simulationModeButton)
      margin = Insets(5,5,5,5)



    val seperationSlider = new Slider(1,100,30):
      this.autosize()

    val avoidanceSliderInBox= new VBox(Label("Avoidance"),seperationSlider)

    val coherenceSlider = new Slider(1,100,20):
      this.autosize()
    val coherenceSliderInBox= new VBox(Label("Coherence"),coherenceSlider)



    val mutationChanceSlider = new Slider(0,1,0.1):
      this.onMouseReleased =  (event) =>
        WORLD.setMutationChance(this.value.get())
      this.autosize()

    var mutationChance = mutationChanceSlider.value.get()
    val mutationChanceLabel = new Label("Mutation chance: " + mutationChance)
    val mutationBox = VBox(mutationChanceLabel, mutationChanceSlider)

    val fovSlider= new Slider(1,360,100):
      this.autosize()

    var fov=fovSlider.value.get().round.toString
    val fovLabel= new Label("FOV: 100")
    val fovBox= new VBox(fovLabel,fovSlider)



    val foodSpawnrateSlider = new Slider(20,300,150):      //todo what values for this
      this.autosize()
    var foodSpawnrate=foodSpawnrateSlider.value.get().toInt
    val foodSpawnrateLabel= new Label("Food spawnrate: "+foodSpawnrate.toString.take(4))
    val foodSpawnrateBox = new VBox(foodSpawnrateLabel,foodSpawnrateSlider)


    val boidShowcase=Canvas(150,170)
    boidShowcase.graphicsContext2D.fill = Color.Blue
    boidShowcase.graphicsContext2D.fillRect(0, 0, 150, 170)




    val sliders = new VBox(10):
      children=Array(avoidanceSliderInBox, coherenceSliderInBox, mutationBox,foodSpawnrateBox,fovBox)

    val settingPane= new VBox(20):
      prefHeight = 700
      prefWidth  = 300
      margin = Insets(10,40,10,10)
      children=Array(HBox(boidShowcase,buttons),togglers,sliders ,logPanel)


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

    foodSpawnrateSlider.onMouseReleased = (event) =
      updateLog("Changed food spawnrate")
      WORLD.setFoodInterval(foodSpawnrateSlider.value.get().toInt)
      foodSpawnrateLabel.text =("Food spawnrate: "+foodSpawnrateSlider.value.get().toInt)
      WORLD.foodTimer=0


    fovSlider.onMouseReleased =  (event) =>
      updateLog("Changed fov")
      fov=fovSlider.value.get().round.toString
      fovLabel.text = ("FOV: "+fov)
      for each <- WORLD.listOfBoids do each.setFov(fovSlider.value.get())


    coherenceSlider.onMouseReleased = (event) =>
      for each <- WORLD.listOfBoids do each.setCoherence(coherenceSlider.value.get())
      updateLog("Changed coherence")

    seperationSlider.onMouseReleased = (event) =>
      updateLog("Changed avoidance")
      for each <- WORLD.listOfBoids do each.setSeperation(seperationSlider.value.get())

    spawnBoids.onMouseReleased = (event) =>
      updateLog("Spawned boid")
      val point=Point(randomSeed.nextDouble()*750,randomSeed.nextDouble()*750)
      val dest= Point(randomSeed.nextDouble()*750,randomSeed.nextDouble()*750)
      val aBoid=Boid(point,dest,WORLD)
      aBoid.setSeperation(seperationSlider.value.get())
      aBoid.setCoherence(coherenceSlider.value.get())
      spawnBoid(aBoid)

    // atm they affect all boids, sould they be made to affect only singles?

  end start


end boidsGUI