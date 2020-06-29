package feederiken

import zio.actors._

package object actors {
  type Worker = ActorRef[Worker.Commands]
}
