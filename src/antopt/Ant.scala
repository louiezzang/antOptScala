package antopt

case class Ant(id: Int, nodes: Map[Int, Point]) {

  val r = scala.util.Random

  override def toString = {
    "Ant " + id
  }

  def findTour(route: List[Int], edges: Map[Edge, EdgeData]): Tour = {
    val possibleRoutes = for (i <- (1 to nodes.size) if (!route.contains(i))) yield Edge(route.head, i)

    possibleRoutes.size match {
      case 0 => {
        val routeLength =
          route.sliding(2, 1)
            .map(segment => nodes(segment.head).distance(nodes(segment.last)))
            .reduceLeft(_ + _)
        Tour(route.last :: route, routeLength + nodes(route.head).distance(nodes(route.last)))
      }
      case _ => {
        val nextNode = findNextNode(possibleRoutes.toList, edges);
        findTour(nextNode :: route, edges)
      }
    }
  }
  
  def findTour(edges: Map[Edge, EdgeData]): Tour = {
    val possibleStarts = for (i <- (1 to nodes.size)) yield Edge(1, i)
    findTour(possibleStarts, edges)
  }

  def findNextNode(possibleRoutes: List[Edge], edges: Map[Edge, EdgeData]) = {
    val probabilities = possibleRoutes.map(route => edges(route).probability)
    val probSize = probabilities.size
    val addedProbabilities = probabilities.drop(1).scanLeft(probabilities(0))(_ + _)
    val addProbSize = addedProbabilities.size
    val maxProbability = addedProbabilities.last
    val targetProbability = r.nextDouble * maxProbability
    val smallerProbabilities = addedProbabilities.filter(x => x <= targetProbability)
    possibleRoutes(smallerProbabilities.size).targetNodeId
  }

}