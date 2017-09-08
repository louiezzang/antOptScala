package antopt
import scala.math._

case class EdgeData(tau: Double, distance: Long) {

  val Alpha: Double = 1
  val Beta: Double = 2
  val Rho: Double = 0.25

  val r = scala.util.Random

  val weightedDistance = pow(distance, Beta)
  val weightedTau = pow(tau, Alpha)
  val probability =
    weightedDistance match {
      case 0 => 0
      case _ => weightedTau / weightedDistance
    }

  def evaporatePheromones: EdgeData = {
    val newTau = tau * (1 - Rho)
    EdgeData(newTau, distance)
  }

  def adjustPheromones(tourLength: Long): EdgeData = {
    assert(tourLength > 0)
    val newTau = tau + (1 / tourLength)
    EdgeData(newTau, distance)
  }
}
  
