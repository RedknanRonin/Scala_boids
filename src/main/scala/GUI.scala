package main
import scalafx.application.JFXApp3
import scalafx.geometry.{Insets, Orientation, Pos}
import scalafx.scene.Scene
import scalafx.scene.canvas.Canvas
import scalafx.scene.control.{Button, ColorPicker, Label, Menu, MenuBar, MenuItem, RadioButton, Separator, Slider, Spinner, ToggleButton, ToggleGroup}
import scalafx.scene.layout.{BorderPane, HBox, VBox}
import scalafx.scene.paint.Color
import scalafx.scene.text.{Font, FontWeight}

import scala.math.{pow, random}



object boidsGUI extends JFXApp3:

  val WORLD = world()
  val canvas = Canvas(WORLD.windowWidth, WORLD.windowHeight)

  def drawBoid(at:Point) =
      canvas.graphicsContext2D.fill = Color.Blue
      canvas.graphicsContext2D.fillOval(at.x,at.y,10,10)

  def tick =
    WORLD.printBoids
    for each <- WORLD.listOfBoids do
      each.move()
      drawBoid(each.pos)

  var paused=true

  def pause()=
    if paused then
      paused=false
    else paused=true

    WORLD.setPause(paused)

  def run() =
    while !paused do
      Thread.sleep(200)
      tick




  def start() =
    stage = new JFXApp3.PrimaryStage:
      title = "Boids"
      width = 1100   //values loosely based on golden ratio
      height = 680
      resizable = false

    val firstLog = new Label(""):
      font = Font("System", FontWeight.ExtraBold, 14)

    val secondLog = new Label("")

    def updateLog(text:String)=
      secondLog.text = firstLog.text.value
      firstLog.text = text

    val logPanel = new VBox:
      prefHeight = 80
      margin = Insets(10)
      children = Array(Separator(Orientation.Horizontal),firstLog, secondLog)


    val pauseButton= new ToggleButton("Run")

    val saveButton = new Button("Save"):
      this.onMouseReleased = (event) => for i<- 0 until 10 do tick

    val loadButton = new Button("Load")

    val spawnBoids= new Button("Spawn boid"):
      this.onMouseReleased = (event) =>
        val point=Point(100,100)
        WORLD.spawnBoid(Boid(point,point.+(100)))
        drawBoid(point)


    val buttons = new VBox(20):
      children=Array(pauseButton,saveButton,loadButton,spawnBoids)
      margin = Insets(5,5,5,20)





    val avoidanceSlider = new Slider(0,1,0.5):
      this.autosize()

    val avoidanceSliderInBox= new VBox(Label("Avoidance"),avoidanceSlider)

    val coherenceSlider = new Slider(0,1,0.5):
      this.autosize()
    val coherenceSliderInBox= new VBox(Label("Coherence"),coherenceSlider)

    val alignmentSlider = new Slider(0,1,0.5):
      this.autosize()
    val alignmentSliderInBox= new VBox(Label("Alignment"),alignmentSlider)

    val mutationChanceSlider = new Slider(0,1,0.1):
      this.onMouseReleased =  (event) =>
        WORLD.setMutationChance(this.value.get())
      this.autosize()

    val fovSlider= new Slider(0,360,180):
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
      children=Array(HBox(boidShowcase,buttons),sliders ,logPanel)

// atm the world is a canvas, might need to be changed.

//todo: chart to show amount of different boids?


    val boidWorld = new HBox:
      children = Array(canvas,Separator(Orientation.Vertical))

    val root = BorderPane(boidWorld, null, settingPane,null , null)
    stage.scene = Scene(parent = root)




    pauseButton.onMouseReleased = (event) =>
      pause()
      if paused then
        pauseButton.text="Run"
        updateLog("Paused")
      else
        pauseButton.text="Pause"
        updateLog("Unpaused")
      run()

    mutationChanceSlider.onMouseReleased  = (event) =>
      mutationChance=mutationChanceSlider.value.get().toString.take(4)
      mutationChanceLabel.text = ("Mutation chance: "+mutationChance)
      updateLog("Changed mutation chance")

    foodSpawnrateSlider.onMouseReleased = (event) =>
      foodSpawnrate=foodSpawnrateSlider.value.get().round.toString.take(4)
      foodSpawnrateLabel.text =("Food spawnrate: "+foodSpawnrate+"/min")

    fovSlider.onMouseReleased =  (event) =>
      fov=fovSlider.value.get().round.toString
      fovLabel.text = ("FOV: "+fov)



  end start


end boidsGUI