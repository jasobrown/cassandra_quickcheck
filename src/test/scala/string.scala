import java.net.InetAddress

import org.scalacheck.Properties
import org.scalacheck.Prop.forAll
import org.scalacheck.Gen

import org.apache.cassandra.gms2.membership.OrswotClock

object OrswotClockSpecification extends Properties("OrswotClock") {

  val randInetAddr = for {
    a  <- Gen.choose(200, 255)
    b <- Gen.choose(200, 255)
    c <- Gen.choose(200, 255)
    d <- Gen.choose(200, 255)
    val addr = InetAddress.getByName(List(a,b,c,d).mkString("."))
  } yield addr


  property("basicGetItToWork") = forAll (randInetAddr) { (addr: InetAddress) =>
    new OrswotClock[InetAddress].getCounter(addr) == null

  }
}
