package antopt

import scala.annotation.tailrec

case class Ant(id: Int) {

  val r = scala.util.Random

  override def toString = {
    "Ant:" + id
  }

  private def chooseNextEdge(possibleNextEdges: List[Edge], allEdges: Map[Edge, EdgeData]) = {
    val probabilities = possibleNextEdges.map(allEdges(_).probability)
    val probSize = probabilities.size
    val addedProbabilities = probabilities.drop(1).scanLeft(probabilities(0))(_ + _)
    val maxProbability = addedProbabilities.last
    val targetProbability = r.nextDouble * maxProbability
    val smallerProbabilities = addedProbabilities.filter(x => x <= targetProbability)
    possibleNextEdges(smallerProbabilities.size)
  }

  @tailrec
  private def runTour(tour: Tour, edges: Map[Edge, EdgeData]): Tour = {
    if (tour.possibleNextEdges.isEmpty) {
      Tour(Edge(tour.route.head.targetNodeId, 1) :: tour.route, tour.nodes)
    } else {
      val nextEdge = chooseNextEdge(tour.possibleNextEdges, edges);
      val newTour = Tour(nextEdge :: tour.route, tour.nodes)
      runTour(newTour, edges)
    }
  }

  def runTour(edges: Map[Edge, EdgeData], nodes: Map[Int, Point]): Tour = {
    runTour(Tour(List[Edge](), nodes), edges)
  }

}