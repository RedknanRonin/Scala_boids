package main

class world {
  val windowHeight= 660
  val windowWidth= 760
  var mutationChance:Double=0

  var listOfBoids=Array[Boid]()


  def setMutationChance(v:Double) = mutationChance=v
  def spawnBoid (b:Boid) = listOfBoids=listOfBoids.appended(b)
  def getBoid(number:Int) =  listOfBoids(number)
  def printBoids= for i <- listOfBoids.indices do println(i +" "+listOfBoids(i))

  def getBoidAt(at:Point) =
    val boidsWithPos:Map[Point,Boid]=listOfBoids.map(a=> a.pos -> a).toMap  // returns a boid at a given point
    boidsWithPos.lift(at).get

  def deleteBoid(at:Point) = listOfBoids.filter(a => a!=getBoidAt(at))

  def tickForTest= for each<- listOfBoids do each.move()

}
