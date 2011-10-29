package scalamatlab;

import matlabcontrol.*;
import matlabcontrol.extensions.MatlabNumericArray;
import matlabcontrol.extensions.MatlabTypeConverter;


public class Matlab {
	private MatlabProxyFactory factory;
	private MatlabProxy proxy; 
	MatlabTypeConverter processor;
	public Matlab(Boolean showMatlab) throws MatlabConnectionException {
		init(showMatlab);
	}
	public Matlab() throws MatlabConnectionException {
		init(false);
	}
	
	public void init(Boolean showMatlab) throws MatlabConnectionException{
		 //Create a proxy, which we will use to control MATLAB
		 MatlabProxyFactoryOptions options = new MatlabProxyFactoryOptions.Builder()
		 .setHidden(!showMatlab)
		 .setProxyTimeout(30000L)
		 .setUsePreviouslyControlledSession(true) 
		 .build();
	    factory = new MatlabProxyFactory(options);
	    proxy = factory.getProxy();
	    processor = new MatlabTypeConverter(proxy);
	}
	
	public void assign(String variable, double[][] ar) throws MatlabInvocationException{
		processor.setNumericArray(variable, new MatlabNumericArray(ar, null));
	}
	public double[][] getArray(String variable) throws MatlabInvocationException{
		return processor.getNumericArray(variable).getRealArray2D();
	}
	public double getDouble(String variable) throws MatlabInvocationException{
		return processor.getNumericArray(variable).getRealArray2D()[0][0];
	}
	
	public void eval(String s) throws MatlabInvocationException {
		proxy.eval(s);
	}	
	public void close(){
		proxy.disconnect();
	}
}