package antopt

import scala.math._
import jawn.ast._
import jawn.ChannelParser

class AntOpt(val nodes: Map[Int, Point], val numAnts: Int = 50, generations: Int = 250) {

  val r = scala.util.Random

  val connections = for (x <- (1 to nodes.size); y <- (1 to nodes.size) if (x != y)) yield Edge(x.toInt, y.toInt)

  val edges = connections.map(connection => (connection, EdgeData(
    r.nextDouble() * 0.1,
    nodes(connection.startNodeId).distance(nodes(connection.targetNodeId)).toInt))).toMap[Edge, EdgeData]

  val bestTour = Tour(List(), nodes)
  
  def adjustAllEdges(edges: Map[Edge, EdgeData], changes:Map[Edge, EdgeData]): Map[Edge, EdgeData] = {
    edges.map(edge => {
      if (changes(edge._1) != None) 
        (edge._1, changes(edge._1))
      else
        edge
    }).toMap[Edge, EdgeData]
  }

//  def adjustPheromones(tours: List[Tour], changes:Map[Edge, EdgeData], edges: Map[Edge, EdgeData]): Map[Edge, EdgeData] = {
//    tours.size match {
//      case 0 => adjustAllEdges(edges, changes)
//      case _ => {
//        tours.head.route.map()
//        adjustPheromones(tours.tail, changes, edges)
//      }
//    }
//  }

  def runTour(generation: Int, edges: Map[Edge, EdgeData], bestTour: Tour): Tour = {
    println("Generation" + generation)
    generation match {
      case 0 => bestTour
      case _ => {
        val tours = (for (i <- (1 to numAnts)) yield Ant(i, nodes).findTour(edges)).toList

        val shortestTour = tours(tours.map(tour => tour.length).zipWithIndex.min._2)
     
        if (shortestTour.length < bestTour.length) {
          println("new shortest Tour in generation " + generation + ": " + shortestTour.length + " : " + shortestTour.route)
        }

        //val adjusted = adjustPheromones(tours.toList, edges, Map[Edge, EdgeData]())
        val evaporated = edges.map(edge => (edge._1, edge._2.evaporatePheromones)).toMap[Edge, EdgeData]

        if (shortestTour.length < bestTour.length)
          runTour(generation - 1, evaporated: Map[Edge, EdgeData], shortestTour: Tour)
        else
          runTour(generation - 1, evaporated: Map[Edge, EdgeData], bestTour: Tour)
      }
    }
  }

  def findBestTour() = {
    val bestTourFound = runTour(generations, edges, bestTour);
    println("bestTour found : " + bestTourFound.length + " : " + bestTourFound.route)
  }

}

object AntOpt extends App {

  def getNodesFromFile(fileName: String) = {
    val json = JParser.parseFromPath(fileName).get
    val coordSection = json.get("NODE_COORD_SECTION")
    val dim = json.get("DIMENSION").asString.toInt
    val nodes = for (x <- (1 to dim)) yield (x, Point(coordSection.get(x.toString).get("x").asString.toInt, coordSection.get(x.toString).get("y").asString.toInt))
    nodes.toMap
  }

  println("Start");
  val nodes = getNodesFromFile("tsmdata/test3.tsp.json")
  val a = new AntOpt(nodes).findBestTour()
  println("End")
}