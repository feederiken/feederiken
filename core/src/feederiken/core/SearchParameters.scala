package feederiken.core

import zio._

case class SearchParameters(
    threadCount: Int,
    gpuAccel: Boolean = true, // soon
)

object SearchParameters {
  def get: URIO[Has[SearchParameters], SearchParameters] = RIO.service
  def threadCount: URIO[Has[SearchParameters], Int] = get.map(_.threadCount)
  def gpuAccel: URIO[Has[SearchParameters], Boolean] = get.map(_.gpuAccel)
}
