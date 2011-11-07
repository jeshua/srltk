package srltk.test.control

import srltk.domains.rlstandard._
import srltk.algs.linear.agents._
object TestSarsaMountainCar {
  import TestRunMountainCar._
  def main(args: Array[String]): Unit = {
    TestRunMountainCar(        
        new EGreedySarsa(MountainCar.dd,.005, .01, 0, .999, 0,  ex),
        episodes=100000
    );
  }
}
