package scalamatlab;
import java.util.Arrays;

import matlabcontrol.*;
import matlabcontrol.extensions.MatlabNumericArray;
import matlabcontrol.extensions.MatlabTypeConverter;
public class Play {

	/**
	 * @param args
	 */
	public static void main(String[] args) throws MatlabConnectionException, MatlabInvocationException{
		
		 //Create a proxy, which we will use to control MATLAB
	    MatlabProxyFactory factory = new MatlabProxyFactory();
	    MatlabProxy proxy = factory.getProxy();

	    //Create and print a 2D double array
	    double[][] array = new double[][] { { 1, 2, 3 }, { 4, 5, 6 }, { 7, 8, 9 } };
	    System.out.println("Original: ");
	    for(int i = 0; i < array.length; i++)
	    {
	        System.out.println(Arrays.toString(array[i]));
	    }
	        
	    //Send the array to MATLAB, transpose it, then retrieve it and convert it to a 2D double array
	    MatlabTypeConverter processor = new MatlabTypeConverter(proxy);
	    processor.setNumericArray("array", new MatlabNumericArray(array, null));
	    proxy.eval("array = transpose(array)");
	    double[][] transposedArray = processor.getNumericArray("array").getRealArray2D();
	        
	     //Print the returned array, now transposed
	     System.out.println("Transposed: ");
	     for(int i = 0; i < transposedArray.length; i++)
	     {
	         System.out.println(Arrays.toString(transposedArray[i]));
	     }
	     proxy.disconnect();

	}
}