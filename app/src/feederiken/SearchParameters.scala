package feederiken

import zio._

case class SearchParameters(
    threadCount: Int,
    gpuAccel: Boolean = true, // soon
)

object SearchParameters {
  def fromCommand(command: Command): URLayer[Services, Has[SearchParameters]] =
    ZLayer.fromEffect {
      for {
        threadCount <- command.j.fold(availableProcessors.map(2 * _))(UIO(_))
      } yield SearchParameters(threadCount)
    }

  def get: URIO[Has[SearchParameters], SearchParameters] = RIO.service
  def threadCount: URIO[Has[SearchParameters], Int] = get.map(_.threadCount)
  def gpuAccel: URIO[Has[SearchParameters], Boolean] = get.map(_.gpuAccel)
}
