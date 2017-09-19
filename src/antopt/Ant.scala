package antopt

import scala.annotation.tailrec

case class Ant(id: Int) {

  val r = scala.util.Random

  override def toString = {
    "Ant:" + id
  }

  private def chooseNextEdge(possibleNextEdges: List[Edge], allEdges: Map[Edge, EdgeData]) = {
    val probabilities = possibleNextEdges.map(edge => allEdges(edge).probability)
    val probSize = probabilities.size
    val addedProbabilities = probabilities.drop(1).scanLeft(probabilities(0))(_ + _)
    val addProbSize = addedProbabilities.size
    val maxProbability = addedProbabilities.last
    val targetProbability = r.nextDouble * maxProbability
    val smallerProbabilities = addedProbabilities.filter(x => x <= targetProbability)
    possibleNextEdges(smallerProbabilities.size)
  }

  @tailrec
  private def runTour(tour: Tour, possibleNextEdges: List[Edge], edges: Map[Edge, EdgeData]): Tour = {
    if (possibleNextEdges.isEmpty) {
      Tour(Edge(tour.route.head.targetNodeId, 1) :: tour.route, tour.nodes)
    } else {
      val nextEdge = chooseNextEdge(possibleNextEdges, edges);
      val newTour = Tour(nextEdge :: tour.route, tour.nodes)
      val newPossibleNextEdges = (for (i <- (1 to tour.nodes.size) if (!newTour.visitedNodes.contains(i))) yield Edge(nextEdge.targetNodeId, i)).toList
      runTour(newTour, newPossibleNextEdges, edges)
    }
  }

  def runTour(edges: Map[Edge, EdgeData], nodes: Map[Int, Point]): Tour = {
    val possibleStartEdges = (for (i <- (1 to nodes.size) if (i != 1)) yield Edge(1, i)).toList
    runTour(Tour(List[Edge](), nodes), possibleStartEdges, edges)
  }

}