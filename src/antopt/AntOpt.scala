package antopt

import scala.math._
import jawn.ast._
import jawn.ChannelParser

class AntOpt(val nodes: Map[Int, Point], val numAnts: Int = 150, generations: Int = 200) {

  val r = scala.util.Random

  val connections = for (x <- (1 to nodes.size); y <- (1 to nodes.size) if (x != y)) yield Edge(x.toInt, y.toInt)

  val edges = connections.map(connection => (connection, EdgeData(
    r.nextDouble() * 0.1,
    nodes(connection.startNodeId).distance(nodes(connection.targetNodeId)).toInt))).toMap[Edge, EdgeData]

  val bestTour = Tour(List(), nodes)

  def createAdjustedEdges(edges: Map[Edge, EdgeData], changes: Map[Edge, EdgeData]): Map[Edge, EdgeData] = {
    edges.map(edge => {
      if (changes.contains(edge._1))
        (edge._1, changes(edge._1))
      else
        edge
    }).toMap[Edge, EdgeData]
  }

  def adjustPheromones(tours: List[Tour], changes: Map[Edge, EdgeData], edges: Map[Edge, EdgeData]): Map[Edge, EdgeData] = {
    tours.size match {
      case 0 => createAdjustedEdges(edges, changes)
      case _ => {
        val adjustedChanges = changes.map(change => {
          if (tours.head.route.contains(change._1)) {
            (change._1, change._2.adjustPheromones(tours.head.length))
          } else {
            change
          }
        }).toMap
        val newChanges = tours.head.route.filter(segment => changes.contains(segment)).map(
          segment => (segment, edges(segment).adjustPheromones(tours.head.length))).toMap

        adjustPheromones(tours.tail, adjustedChanges ++ newChanges, edges)
      }
    }
  }

  def adjustPheromones(tours: List[Tour], edges: Map[Edge, EdgeData]): Map[Edge, EdgeData] = {
    adjustPheromones(tours.tail, Map[Edge, EdgeData](), edges)
  }

  def runToursWithMultipleAnts(generation: Int, edges: Map[Edge, EdgeData], bestTour: Tour): Tour = {
    println("Generation" + generation)
    generation match {
      case 0 => bestTour
      case _ => {
        val tours = (1 to numAnts).par.map(i => Ant(i).runTour(edges, nodes)).toList

        val shortestTour = tours(tours.map(tour => tour.length).zipWithIndex.min._2)

        if (shortestTour.length < bestTour.length) {
          println("new shortest Tour in generation " + generation + ": " + shortestTour.length + " : " + shortestTour.route)
        }

        val adjusted = adjustPheromones(tours.toList, edges)
        val evaporated = adjusted.map(edge => (edge._1, edge._2.evaporatePheromones)).toMap[Edge, EdgeData]

        if (shortestTour.length < bestTour.length)
          runToursWithMultipleAnts(generation - 1, evaporated: Map[Edge, EdgeData], shortestTour: Tour)
        else
          runToursWithMultipleAnts(generation - 1, evaporated: Map[Edge, EdgeData], bestTour: Tour)
      }
    }
  }

  def findBestTour() = {
    val bestTourFound = runToursWithMultipleAnts(generations, edges, bestTour);
    println("bestTour found : " + bestTourFound.length + " : " + bestTourFound.route.reverse)
  }

}

object AntOpt extends App {

  def getNodesFromFile(fileName: String) = {
    val json = JParser.parseFromPath(fileName).get
    val coordSection = json.get("NODE_COORD_SECTION")
    val dim = json.get("DIMENSION").asString.toInt
    (for (i <- (0 to dim - 1)) yield (i + 1, Point(coordSection.get(i).get("x").asString.toInt, coordSection.get(i).get("y").asString.toInt))).toMap
  }

  if (args.length == 0) {
    println("Please pass a valid tsm json file")
  }
  val filename = args(0)

  println("Start");
  val nodes = getNodesFromFile(filename)
  val a = new AntOpt(nodes).findBestTour()
  println("End")
}