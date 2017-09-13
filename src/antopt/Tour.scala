package antopt

case class Tour(route: List[Edge], nodes: Map[Int, Point]) {
  def length =
    route match {
      case head :: _ => route.map(segment => nodes(segment.startNodeId).distance(nodes(segment.targetNodeId))).reduceLeft(_ + _)
      case _         => Long.MaxValue
    }

  def visitedNodes =
    route match {
      case head :: _ => head.targetNodeId :: route.map(edge => edge.startNodeId)
      case _         => List[Int]()
    }
}

