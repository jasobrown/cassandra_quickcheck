import java.net.InetAddress

import org.apache.cassandra.gms2.membership.{OrswotClock, Orswot}
import org.scalacheck.commands.Commands
import org.scalacheck.{Prop, Gen, Properties}

import scala.util.Try

object OrswotCommands extends Properties("OrswotCommands") {
  property("orswotProps") = OrswotSpecification.property()
}

object OrswotSpecification extends Commands {

  /**
   * Contains a reference to the state 'before' the test functions execute, that way we can compare the
   * before and after states of the Orswot to ensure the state has transitioned properly
   * @param before A snapshot of the Orswot
   */
  case class State (before: Orswot[InetAddress, InetAddress], current: Orswot[InetAddress, InetAddress])
  {

  }

  class OrswotWrapper(val orswot: Orswot[InetAddress, InetAddress])
  {
    //TODO: not sure why i need this accessor method ...
    def getIt() = orswot
  }

  val localAddr = InetAddress.getByName("127.0.0.1")
  override type Sut = OrswotWrapper

  override def initialPreCondition(state: State): Boolean = true

  override def genInitialState: Gen[State] = {
    val orswot = new Orswot[InetAddress, InetAddress](localAddr)
    State(new Orswot[InetAddress, InetAddress](orswot), orswot)
  }

  override def newSut(state: State): OrswotWrapper = new OrswotWrapper(state.current)

  override def canCreateNewSut(newState: State, initSuts: Traversable[State], runningSuts: Traversable[OrswotWrapper]): Boolean = true

  override def destroySut(orswot: OrswotWrapper): Unit = {}

  override def genCommand(state: State): Gen[OrswotSpecification.Command] = {
    if (state.current.getElements.isEmpty)
      genAdd(state)
    else
     Gen.frequency((1, genAdd(state))
    //               (10, genApplyAdd(state))
      //                (1, genRemove)
      //                (3, genApplyRemove)
    )
  }

  val genSeed = Gen.oneOf(localAddr, InetAddress.getByName("127.0.1.0"), InetAddress.getByName("127.0.1.1"))
  val genPeer = Gen.oneOf(InetAddress.getByName("127.0.1.0"), InetAddress.getByName("127.0.1.1"), InetAddress.getByName("127.0.1.2"), InetAddress.getByName("127.0.1.3"),
                          InetAddress.getByName("127.0.1.4"), InetAddress.getByName("127.0.1.5"), InetAddress.getByName("127.0.1.6"), InetAddress.getByName("127.0.1.7"))

  def genAdd(state: State): Gen[Add]= for {
    peer <- genPeer
    seed <- genSeed
  } yield Add(peer, seed)

  def genApplyAdd(state: State): Gen[ApplyAdd] = for {
    peer <- genPeer
    //clock = (state.current.getClock(peer) != null ? state.current.getClock(peer) : )
  } yield ApplyAdd(peer)

//  val genRemove: Gen[Remove] = Gen.resultOf(Remove)
//  val genApplyRemove: Gen[ApplyRemove] = Gen.resultOf(ApplyRemove)

  case class Add(peer: InetAddress, seed: InetAddress) extends Command {
    override type Result = OrswotClock[InetAddress]

    override def preCondition(state: State): Boolean =
      state.current.getClock(peer) == state.before.getClock(peer)

    override def run(wrapper: OrswotWrapper): Result = {
      wrapper.getIt().add(peer, seed)
    }

    override def postCondition(state: State, result: Try[OrswotClock[InetAddress]]): Prop = {
      println(state)
      (state.before.getClock(peer) == null && result.isSuccess) || result.get.dominates(state.before.getClock(peer))
    }

    override def nextState(state: State): State = {
      new State(new Orswot[InetAddress, InetAddress](state.current), state.current)
    }
  }

  case class ApplyAdd(peer: InetAddress) extends Command {
    override type Result = OrswotClock[InetAddress]

    override def preCondition(state: State): Boolean =
      state.current.getClock(peer) == state.before.getClock(peer)

    override def run(wrapper: OrswotWrapper): Result = {
      // take the easy case for now, just use the existing clock from the orswot
      // TODO: create (disjoint) clocks - especially if there's nothing in the orswot already
      val orswot = wrapper.getIt()
      orswot.applyAdd(peer, orswot.getClock)

      // clock should be updated after the call to applyAll
      orswot.getClock
    }

    override def postCondition(state: State, result: Try[OrswotClock[InetAddress]]): Prop = {
      (state.before.getClock(peer) == null && result.isSuccess) || result.get.dominates(state.before.getClock(peer))
    }

    override def nextState(state: State): State = {
      new State(new Orswot[InetAddress, InetAddress](state.current), state.current)
    }
  }

  /*
    case class ApplyAdd(orswot: Orswot[InetAddress, InetAddress]) extends Command {
      override type Result = this.type

      override def preCondition(state: State): Boolean = ???

      override def postCondition(state: State, result: Try[Result]): Prop = ???

      override def run(sut: Orswot[InetAddress, InetAddress]): Result = ???

      override def nextState(state: State): State = ???
    }

    case class Remove(orswot: Orswot[InetAddress, InetAddress]) extends Command {
      override type Result = this.type

      override def preCondition(state: State): Boolean = ???

      override def postCondition(state: State, result: Try[Result]): Prop = ???

      override def run(sut: Orswot[InetAddress, InetAddress]): Result = ???

      override def nextState(state: State): State = ???
    }

    case class ApplyRemove(orswot: Orswot[InetAddress, InetAddress]) extends Command {
      override type Result = this.type

      override def preCondition(state: State): Boolean = ???

      override def postCondition(state: State, result: Try[Result]): Prop = ???

      override def run(sut: Orswot[InetAddress, InetAddress]): Result = ???

      override def nextState(state: State): State = ???
    }
  */
}
