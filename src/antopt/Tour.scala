package antopt

case class Tour(route: List[Edge], nodes: Map[Int, Point]) {
  val length =
    route match {
      case head :: _ => route.map(segment => nodes(segment.startNodeId).distance(nodes(segment.targetNodeId))).reduceLeft(_ + _)
      case _         => Long.MaxValue
    }

  val visitedNodes =
    route match {
      case head :: _ => head.targetNodeId :: route.map(_.startNodeId)
      case _         => List[Int]()
    }

  val possibleNextEdges =
    route match {
      case head :: _ => (for (i <- (1 to nodes.size) if (!visitedNodes.contains(i))) yield Edge(route.head.targetNodeId, i)).toList
      case _         => (for (i <- (1 to nodes.size) if (i != 1)) yield Edge(1, i)).toList
    }
}

