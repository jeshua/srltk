package scalamatlab;
import org.netlib.blas.*;
import java.lang.reflect.*;
import java.util.Random;


public class NetlibTest {
	public static void main(String[] args){				
		try{
			//get java blas object and make it accessible
			Class javaBlasClass = Class.forName("org.netlib.blas.JBLAS");
			Field javaBlas = javaBlasClass.getDeclaredField("INSTANCE");
			Field jInstance = javaBlas.getClass().getDeclaredField("modifiers");
			jInstance.setAccessible(true);
			jInstance.setInt(javaBlas,javaBlas.getModifiers() & ~Modifier.FINAL);
			javaBlas.setAccessible(true);

			//get native blas object and make it accessible
			Class nativeBlasClass = Class.forName("org.netlib.blas.NativeBLAS");
			Field nativeBlas = nativeBlasClass.getDeclaredField("INSTANCE");
			Field nInstance = nativeBlas.getClass().getDeclaredField("modifiers");
			nInstance.setAccessible(true);
			nInstance.setInt(nativeBlas,nativeBlas.getModifiers() & ~Modifier.FINAL);
			nativeBlas.setAccessible(true);

			//get blas current object and make it accessible
			Field blasCurrent = Class.forName("org.netlib.blas.BLAS").getDeclaredField("current");
			Field bInstance = blasCurrent.getClass().getDeclaredField("modifiers");
			bInstance.setAccessible(true);
			bInstance.setInt(blasCurrent, blasCurrent.getModifiers() & ~Modifier.FINAL);
			blasCurrent.setAccessible(true);



			int[] szs = {100, 200, 500, 800, 1000, 2000, 3000, 4000};
			for(int k = 0;k<szs.length;k++){
				int sz = szs[k];

				double[] mat1 = new double[sz*sz];
				double[] mat2 = new double[sz*sz];
				double[] matOut = new double[sz*sz];
				Random rand = new Random();
				for(int i=0;i<sz*sz;i++){
					mat1[i] = rand.nextDouble();
					mat2[i] = rand.nextDouble();
					matOut[i] = 0;
				}				
				//SET TO JBLAS
				blasCurrent.set(null, javaBlas.get(null));
				
				long time1 = System.nanoTime();//bean.getCurrentThreadUserTime();	
				BLAS.getInstance().dgemm("N", "N",
						sz, sz, sz,
						1.0, mat1, sz, mat2, sz,
						0.0, matOut, sz);		
				double elapsed = (System.nanoTime() - time1)/1000000000d;
				System.out.printf("JBLAS:\t\t %.4f seconds to multiply %dx%d matrices.\n",elapsed,sz,sz);							
				//SET TO NativeBLAS
				blasCurrent.set(null, nativeBlas.get(null));				
				time1 = System.nanoTime();
				BLAS.getInstance().dgemm("N", "N",
						sz, sz, sz,
						1.0, mat1, sz, mat2, sz,
						0.0, matOut, sz);		
				elapsed = (System.nanoTime() - time1)/1000000000d;
				System.out.printf("NativeBLAS:\t %.4f seconds to multiply %dx%d matrices.\n",elapsed,sz,sz);
			}
		}catch(Exception e){e.printStackTrace();}
	}
}
