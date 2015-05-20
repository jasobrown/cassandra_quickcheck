import java.net.InetAddress
import java.util

import org.scalacheck.Properties
import org.scalacheck.Prop.forAll
import org.scalacheck.Gen

import org.apache.cassandra.gms2.membership.OrswotClock

/**
 * Rookie attempt at quickcheck tests for OrswotClock.
 */
object BasicOrswotClockSpecification extends Properties("OrswotClock") {

  val randCounter = for {
    counter <- Gen.choose(1, 15)
  } yield new Integer(counter)

  val randInetAddr = for {
    a  <- Gen.choose(1, 255)
    b <- Gen.choose(0, 255)
    c <- Gen.choose(0, 255)
    d <- Gen.choose(0, 255)
  } yield InetAddress.getByName(List(a,b,c,d).mkString("."))

//  val randInetAddrMap = for {
//    size <- Gen.choose(1, 12)
//   addrs = Gen.mapOfN(size, Gen.(randInetAddr, Gen.choose(1, 8)))
//
//  } yield size

  property("basicGetItToWork") = forAll (randInetAddr) { (addr: InetAddress) =>
    new OrswotClock[InetAddress].getCounter(addr) == null
  }

  property("clockReadForAddr") = forAll (randInetAddr :| "InetAddr", randCounter :| "Counter")
  { (addr: InetAddress, counter: Integer) =>
    val clock = new util.HashMap[InetAddress, Integer]()
    clock.put(addr, counter)
    val orswotClock = new OrswotClock[InetAddress](clock)
    orswotClock.getCounter(addr) == counter
  }

  property("incrementClock") = forAll (randInetAddr :| "InetAddr", randCounter :| "Counter")
  { (addr: InetAddress, counter: Integer) =>
    val clock = new util.HashMap[InetAddress, Integer]()
    clock.put(addr, counter)
    val orswotClock = new OrswotClock[InetAddress](clock)
    orswotClock.increment(addr).getCounter(addr) == counter + 1
  }

  property("descendsDominates") = forAll (randInetAddr :| "InetAddr", randCounter :| "Counter")
  { (addr: InetAddress, counter: Integer) =>
    val clock = new util.HashMap[InetAddress, Integer]()
    clock.put(addr, counter)
    val orswotClock = new OrswotClock[InetAddress](clock)
    val nextClock = orswotClock.increment(addr)
    nextClock.descends(orswotClock) && nextClock.dominates(orswotClock)
  }

  property("mergeClocks") = forAll (randInetAddr :| "InetAddr1", randCounter :| "Counter1", randInetAddr :| "InetAddr2", randCounter :| "Counter2")
  { (addr1: InetAddress, counter1: Integer, addr2: InetAddress, counter2: Integer) =>
    val clock1 = new util.HashMap[InetAddress, Integer]()
    clock1.put(addr1, counter1)
    val orswotClock1 = new OrswotClock[InetAddress](clock1)

    val clock2 = new util.HashMap[InetAddress, Integer]()
    clock2.put(addr2, counter2)
    val orswotClock2 = new OrswotClock[InetAddress](clock2)

    val nextClock = orswotClock1.merge(orswotClock2)
    nextClock.dominates(orswotClock1) && nextClock.dominates(orswotClock2)
  }
}
