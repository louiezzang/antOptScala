package antopt

case class Tour(route: List[Int], length: Long) {
  def segments = route.sliding(2, 1).toList.map(segment => (segment.head, segment.tail.head))
}
  
