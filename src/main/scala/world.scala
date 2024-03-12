package main

class world {
  val windowHeight= 750
  val windowWidth= 750
  var mutationChance:Double=0
  private var paused: Boolean = false

  var listOfBoids=Array[Boid]()

  def setPause(v:Boolean) = paused=v
  def setMutationChance(v:Double) = mutationChance=v
  def spawnBoid (b:Boid) = listOfBoids=listOfBoids.appended(b)
  def getBoid(number:Int) =  listOfBoids(number)
  def printBoids= for i <- listOfBoids.indices do println(i +" "+listOfBoids(i))


  def tick()=
    println("boids: " + listOfBoids.length)
    this.printBoids
    for each <- listOfBoids do
      each.move()
      boidsGUI.drawBoidAt(each.pos)



}
