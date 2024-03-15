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

import scala.math.pow
import scala.util.Random


object boidsGUI extends JFXApp3:

  val WORLD = world()
  val canvas = Canvas(WORLD.windowWidth, WORLD.windowHeight)
  val gc=canvas.graphicsContext2D
  val randomSeed= Random(69420)
  var drawViewLine: Boolean= true
  var paused=true
  var drawViewCircle = true

  def drawBoid(boid:Boid) =
      val (at,dest,fov) = (boid.pos,boid.acceleration,boid.fov)
      gc.fill = Color.Blue
      gc.fillOval(at.x,at.y,10,10)
      if drawViewLine then gc.strokeLine(at.x+5,at.y+5,dest.x,dest.y)
      if drawViewCircle then gc.strokeOval(at.x-fov/2+5,at.y-fov/2+5,fov,fov)


  def tick =
    for each <- WORLD.listOfBoids do
      drawBoid(each)
      each.move()

  def printDebug =
    println("\nBoids: "+WORLD.listOfBoids.length)
    println("****************************")
    for eah<- WORLD.listOfBoids do println(eah)

  var lastTime = System.nanoTime()

  val timer = AnimationTimer(time => {
    val deltaTime = (time - lastTime)
    lastTime = time
    gc.clearRect(0,0,canvas.width.value,canvas.height.value)
    tick

  })

  def pause()=
    if paused then
      paused=false
    else paused=true
    WORLD.setPause(paused)

//todo: Why do they eat each other

  def start() =
    stage = new JFXApp3.PrimaryStage:
      title = "Boids"
      width = 1100   //values loosely based on golden ratio
      height = 680
      resizable = false

    val firstLog = new Label(""):
      font = Font("System", FontWeight.ExtraBold, 14)
    val secondLog = new Label("")
    val thirdLog = new Label ("")



    def updateLog(text:String)=
      thirdLog.text=secondLog.text.value
      secondLog.text = firstLog.text.value
      firstLog.text = text

    val logPanel = new VBox:
      margin = Insets(10)
      children = Array(Separator(Orientation.Horizontal),firstLog, secondLog,thirdLog)


    val pauseButton= new ToggleButton("Run")

    val saveButton = new Button("Save")
    saveButton.onMouseReleased = (event) => printDebug


    val loadButton = new Button("Load")

    val spawnBoids= new Button("Spawn boid"):
      this.onMouseReleased = (event) =>
        updateLog("Spawned boid")
        val point=Point(randomSeed.nextDouble()*750,randomSeed.nextDouble()*750)
        val dest= Point(randomSeed.nextDouble()*750,randomSeed.nextDouble()*750)
        WORLD.spawnBoid(Boid(point,dest,WORLD))
        drawBoid(Boid(point,dest,WORLD))

    val buttons = new VBox(20):
      children=Array(pauseButton,saveButton,loadButton,spawnBoids)
      margin = Insets(5,5,5,20)


    val fovToggler = new Button("Show fov")
    fovToggler.onMouseReleased = (event) => drawViewCircle= !drawViewCircle

    val lineToggler = new Button("Show direction")
    lineToggler.onMouseReleased = (event)=> drawViewLine= !drawViewLine

    val boidCount = new Button("Boids: "+WORLD.listOfBoids.length)
    boidCount.onMouseReleased = (event)=> boidCount.text=("Boids: "+WORLD.listOfBoids.length)



    val togglers = new HBox(10):
      children = Array(fovToggler,lineToggler,boidCount)
      margin = Insets(5,5,5,5)



    val avoidanceSlider = new Slider(0,30,10):
      this.autosize()

    val avoidanceSliderInBox= new VBox(Label("Avoidance"),avoidanceSlider)

    val coherenceSlider = new Slider(0,30,10):
      this.autosize()
    val coherenceSliderInBox= new VBox(Label("Coherence"),coherenceSlider)

    val alignmentSlider = new Slider(0,30,10):
      this.autosize()
    val alignmentSliderInBox= new VBox(Label("Alignment"),alignmentSlider)

    val mutationChanceSlider = new Slider(0,1,0.1):
      this.onMouseReleased =  (event) =>
        WORLD.setMutationChance(this.value.get())
      this.autosize()

    val fovSlider= new Slider(1,360,180):
      this.autosize()

    var fov=fovSlider.value.get().round.toString
    val fovLabel= new Label("FOV: 180")
    val fovBox= new VBox(fovLabel,fovSlider)

    var mutationChance="0.1"
    val mutationChanceLabel= new Label("Mutation chance: "+mutationChance)
    val mutationBox = VBox(mutationChanceLabel,mutationChanceSlider)

    val foodSpawnrateSlider = new Slider(0,60,3):      // food/60s? Random times??
      this.autosize()
    var foodSpawnrate="10"
    val foodSpawnrateLabel= new Label("Food spawnrate: "+foodSpawnrate.toString.take(4)+"/min")
    val foodSpawnrateBox = new VBox(foodSpawnrateLabel,foodSpawnrateSlider)


    val boidShowcase=Canvas(150,170)
    boidShowcase.graphicsContext2D.fill = Color.Blue
    boidShowcase.graphicsContext2D.fillRect(0, 0, 150, 170)




    val sliders = new VBox(10):
      children=Array(avoidanceSliderInBox, coherenceSliderInBox,alignmentSliderInBox, mutationBox,foodSpawnrateBox,fovBox)

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
      mutationChance=mutationChanceSlider.value.get().toString.take(4)
      mutationChanceLabel.text = ("Mutation chance: "+mutationChance)
      updateLog("Changed mutation chance")

    foodSpawnrateSlider.onMouseReleased = (event) =>
      updateLog("Changed food spawnrate")
      foodSpawnrate=foodSpawnrateSlider.value.get().round.toString.take(4)
      foodSpawnrateLabel.text =("Food spawnrate: "+foodSpawnrate+"/min")

    fovSlider.onMouseReleased =  (event) =>
      updateLog("Changed fov")
      fov=fovSlider.value.get().round.toString
      fovLabel.text = ("FOV: "+fov)
      for each <- WORLD.listOfBoids do each.setFov(fovSlider.value.get())

    alignmentSlider.onMouseReleased = (event) =>
      for each <- WORLD.listOfBoids do
      each.setAlignment(alignmentSlider.value.get())
      updateLog("Changed alignment")
    coherenceSlider.onMouseReleased = (event) =>
      for each <- WORLD.listOfBoids do each.setCoherence(coherenceSlider.value.get())
      updateLog("Changed coherence")

    avoidanceSlider.onMouseReleased = (event) =>
      updateLog("Changed avoidance")
      for each <- WORLD.listOfBoids do each.setSeperation(avoidanceSlider.value.get())


    // atm they affect all boids, sould they be made to affect only singles?

  end start


end boidsGUI