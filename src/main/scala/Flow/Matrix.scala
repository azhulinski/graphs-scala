package Flow

sealed trait Matrix[V] {
  def getIndex(n: V): Int

  def neighbours(n: V): Vector[V]

  def neighboursWeights(n: V): Vector[Int]

  def edgeAt(n: V, m: V): Int

  def update(n: V, m: V, w: Int): Matrix[V]
}

case class AdjMatrix[V](vertices: List[V], weights: Vector[Int], size: Int) extends Matrix[V] {

  def getIndex(v: V): Int = vertices.indexOf(v)

  def neighbours(v: V): Vector[V] = {
    vertices.zip(neighboursWeights(v)).flatMap {
      case (neighbour, weight) if weight > 0 => Some(neighbour)
      case _ => None
    }.toVector
  }

  def neighboursWeights(v: V): Vector[Int] = {
    val i = getIndex(v)
    weights.slice(i * size, i * size + size)
  }

  def edgeAt(v: V, m: V): Int = {
    val i = getIndex(v)
    val j = getIndex(m)
    weights(i * size + j)
  }

  def update(v: V, m: V, w: Int): Matrix[V] = {
    val i = getIndex(v)
    val j = getIndex(m)
    AdjMatrix(vertices, weights.updated(i * size + j, w), size)
  }

}
