package feederiken

import zio.actors._

package object actors {
  type Collector = ActorRef[Collector.Commands]
  type Dispatcher = ActorRef[Dispatcher.Commands]
  type Saver = ActorRef[Saver.Commands]
  type Worker = ActorRef[Worker.Commands]
}
