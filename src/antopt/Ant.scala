package antopt

case class Ant(id: Int, nodes: Map[Int, Point]) {

  val r = scala.util.Random

  override def toString = {
    "Ant " + id
  }

  def findTour(tour: Tour, possibleNextEdges: List[Edge], edges: Map[Edge, EdgeData]): Tour = {
    if (possibleNextEdges.isEmpty) {
      Tour(Edge(tour.route.head.targetNodeId, 1) :: tour.route, tour.nodes)
    } else {
      val nextEdge = findNextEdge(possibleNextEdges, edges);
      val newTour = Tour(nextEdge :: tour.route, nodes)
      val newPossibleNextEdges = (for (i <- (1 to nodes.size) if (!newTour.visitedNodes.contains(i))) yield Edge(nextEdge.targetNodeId, i)).toList
      findTour(newTour, newPossibleNextEdges, edges)
    }
  }

  def findTour(edges: Map[Edge, EdgeData]): Tour = {
    val possibleStartEdges = (for (i <- (1 to nodes.size) if (i != 1)) yield Edge(1, i)).toList
    findTour(Tour(List[Edge](), nodes), possibleStartEdges, edges)
  }

  def findNextEdge(possibleNextEdges: List[Edge], allEdges: Map[Edge, EdgeData]) = {
    val probabilities = possibleNextEdges.map(edge => allEdges(edge).probability)
    val probSize = probabilities.size
    val addedProbabilities = probabilities.drop(1).scanLeft(probabilities(0))(_ + _)
    val addProbSize = addedProbabilities.size
    val maxProbability = addedProbabilities.last
    val targetProbability = r.nextDouble * maxProbability
    val smallerProbabilities = addedProbabilities.filter(x => x <= targetProbability)
    possibleNextEdges(smallerProbabilities.size)
  }

}