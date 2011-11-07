package srltk.test.control

import srltk.domains.rlstandard._
import srltk.algs.linear.agents.EGreedySarsa

object TestSarsaPuddleWorld {
  import PuddleWorld._
  def main(args: Array[String]): Unit = {
    TestRunPuddleWorld(        
        new EGreedySarsa(PuddleWorld.dd,.01, .01, 0, .99, 0, TestRunPuddleWorld.ex),
        episodes=1000
    );
  }
}