package scalamatlab

import scalala.tensor._
import matlabcontrol._
import matlabcontrol.extensions._

object ScalalaMatlab {
	var factory : MatlabProxyFactory = null
	var proxy :  MatlabProxy  = null
	def init() = {
	  factory = new MatlabProxyFactory();
	 proxy = factory.getProxy();
	}
  
}
