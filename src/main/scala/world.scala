package main

class world {
  val windowHeight= 680
  val windowWidth= 745
  var mutationChance:Double=0
  private var paused: Boolean = false

  var listOfBoids=Array[Boid]()

  def setPause(v:Boolean) = paused=v
  def setMutationChance(v:Double) = mutationChance=v
  def spawnBoid (b:Boid) = listOfBoids=listOfBoids.appended(b)
  def getBoid(number:Int) =  listOfBoids(number)
  def printBoids= for i <- listOfBoids.indices do println(i +" "+listOfBoids(i))

  def getBoidAt(at:Point) =
    val boidsWithPos:Map[Point,Boid]=listOfBoids.map(a=> a.pos -> a).toMap  // returns a boid at a given point
    boidsWithPos.lift(at).get

  def deleteBoid(at:Point) = listOfBoids.filter(a => a!=getBoidAt(at))

}
