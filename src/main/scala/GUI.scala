import scalafx.application.JFXApp3
import scalafx.geometry.{Insets, Orientation, Pos}
import scalafx.scene.Scene
import scalafx.scene.canvas.Canvas
import scalafx.scene.control.{Button, ColorPicker, Label, Menu, MenuBar, MenuItem, RadioButton, Separator, Slider, Spinner, ToggleGroup}
import scalafx.scene.layout.{BorderPane, HBox, VBox}
import scalafx.scene.paint.Color
import scalafx.scene.text.{Font, FontWeight}

object boidsGUI extends JFXApp3:

  def start() =
    stage = new JFXApp3.PrimaryStage:
      title = "Boids"
      width = 1100
      height = 700
      resizable = false


    val firstLog = new Label("Log: This place should display the log"):
      font = Font("System", FontWeight.ExtraBold, 14)

    val secondLog = new Label("The second newest log should be displayed here.")


    val logPanel = new VBox:
      prefHeight = 80
      margin = Insets(10)
      children = Array(Separator(Orientation.Horizontal),firstLog, secondLog)



    val pauseButton= new Button("Pause"):
      defaultButton=true

    val saveButton = new Button("Save")

    val loadButton = new Button("Load")

    val saveAndPause = new HBox(20):
      children=Array(pauseButton,saveButton,loadButton)


    val avoidanceSlider = new Slider(0,1,0.5):
      this.autosize()
    val avoidanceSliderInBox= new VBox(Label("Avoidance"),avoidanceSlider)

    val coherenceSlider = new Slider(0,1,0.5):
      this.autosize()
    val coherenceSliderInBox= new VBox(Label("Coherence"),coherenceSlider)

    val alignmentSlider = new Slider(0,1,0.5):
      this.autosize()
    val alignmentSliderInBox= new VBox(Label("Alignment"),alignmentSlider)

    val boidShowcase=Canvas(200,300)

    boidShowcase.graphicsContext2D.fill = Color.Blue
    boidShowcase.graphicsContext2D.fillRect(150, 150, 50, 20)


    val settingPane= new VBox(20):
      prefHeight = 700
      prefWidth  = 300
      margin = Insets(10,40,10,10)
      children=Array(boidShowcase,saveAndPause, avoidanceSliderInBox, coherenceSliderInBox,alignmentSliderInBox,logPanel)

// atm the world is a canvas, might need to be changed.

//todo: chart to show amount of different boids?

    val canvas = Canvas(750, 500)
    canvas.graphicsContext2D
    canvas.graphicsContext2D.fill = Color.Blue
    canvas.graphicsContext2D.fillRect(50, 50, 20, 20)
    canvas.graphicsContext2D.fill = Color.ForestGreen
    canvas.graphicsContext2D.fillOval(350, 270, 10, 10)

    val boidWorld = new HBox:
      children = Array(canvas,Separator(Orientation.Vertical))

    val root = BorderPane(boidWorld, null, settingPane,null , null)
    stage.scene = Scene(parent = root)
  end start
end boidsGUI