
/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.apache.commons.math3.linear;

import java.io.Serializable;

import org.apache.commons.math3.exception.NumberIsTooLargeException;
import org.apache.commons.math3.util.OpenIntToDoubleHashMap;

/**
 * Sparse matrix implementation based on an open addressed map.
 *
 * @version $Id: OpenMapRealMatrix.java 1244107 2012-02-14 16:17:55Z erans $
 * @since 2.0
 */
public class OMRealMatrix extends AbstractRealMatrix
    implements SparseRealMatrix, Serializable {
    /** Serializable version identifier. */
    private static final long serialVersionUID = -5962461716457143437L;
    /** Number of rows of the matrix. */
    private final int rows;
    /** Number of columns of the matrix. */
    private final int columns;
    /** Storage for (sparse) matrix elements. */
    private final OpenIntToDoubleHashMap entries;    
  
    public class OMRealMatrixIterator implements java.util.Iterator<MatrixEntry>{
    	private OpenIntToDoubleHashMap.Iterator it;
    	public OMRealMatrixIterator(){
    	    it = entries.iterator();
    	}
		@Override
		public boolean hasNext() {			
			return it.hasNext();
		}

		@Override
		public MatrixEntry next() {
			it.advance();
			int key      = it.key();
			int r        = key / columns;
			int c        = key % columns;			
			return new MatrixEntry(r,c,it.value());
		}

		@Override
		public void remove() {}    	
    }
    
    public OMRealMatrixIterator iterator(){
    	return new OMRealMatrixIterator();
    }
    
    /**
     * Build a sparse matrix with the supplied row and column dimensions.
     *
     * @param rowDimension Number of rows of the matrix.
     * @param columnDimension Number of columns of the matrix.
     */
    public OMRealMatrix(int rowDimension, int columnDimension) {
        super(rowDimension, columnDimension);
        long lRow = rowDimension;
        long lCol = columnDimension;
        if (lRow * lCol >= Integer.MAX_VALUE) {
            throw new NumberIsTooLargeException(lRow * lCol, Integer.MAX_VALUE, false);
        }
        this.rows = rowDimension;
        this.columns = columnDimension;
        this.entries = new OpenIntToDoubleHashMap(0.0);
    }

    /**
     * Build a matrix by copying another one.
     *
     * @param matrix matrix to copy.
     */
    public OMRealMatrix(OMRealMatrix matrix) {
        this.rows = matrix.rows;
        this.columns = matrix.columns;
        this.entries = new OpenIntToDoubleHashMap(matrix.entries);
    }

    /** {@inheritDoc} */
    @Override
    public OMRealMatrix copy() {
        return new OMRealMatrix(this);
    }

    /** {@inheritDoc} */
    @Override
    public OMRealMatrix createMatrix(int rowDimension, int columnDimension) {
        return new OMRealMatrix(rowDimension, columnDimension);
    }

    /** {@inheritDoc} */
    @Override
    public int getColumnDimension() {
        return columns;
    }

    /**
     * Compute the sum of this matrix and {@code m}.
     *
     * @param m Matrix to be added.
     * @return {@code this} + {@code m}.
     * @throws org.apache.commons.math3.exception.DimensionMismatchException
     * if {@code m} is not the same size as this matrix.
     */
    public OMRealMatrix add(OMRealMatrix m) {

        // safety check
        MatrixUtils.checkAdditionCompatible(this, m);

        final OMRealMatrix out = new OMRealMatrix(this);
        for (OpenIntToDoubleHashMap.Iterator iterator = m.entries.iterator(); iterator.hasNext();) {
            iterator.advance();
            final int row = iterator.key() / columns;
            final int col = iterator.key() - row * columns;
            out.setEntry(row, col, getEntry(row, col) + iterator.value());
        }

        return out;

    }

    /** {@inheritDoc} */
    @Override
    public OMRealMatrix subtract(final RealMatrix m) {
        try {
            return subtract((OMRealMatrix) m);
        } catch (ClassCastException cce) {
            return (OMRealMatrix) super.subtract(m);
        }
    }
    
    

    /**
     * Subtract {@code m} from this matrix.
     *
     * @param m Matrix to be subtracted.
     * @return {@code this} - {@code m}.
     * @throws org.apache.commons.math3.exception.DimensionMismatchException
     * if {@code m} is not the same size as this matrix.
     */
    public OMRealMatrix subtract(OMRealMatrix m) {
        // Safety check.
        MatrixUtils.checkAdditionCompatible(this, m);

        final OMRealMatrix out = new OMRealMatrix(this);
        for (OpenIntToDoubleHashMap.Iterator iterator = m.entries.iterator(); iterator.hasNext();) {
            iterator.advance();
            final int row = iterator.key() / columns;
            final int col = iterator.key() - row * columns;
            out.setEntry(row, col, getEntry(row, col) - iterator.value());
        }

        return out;
    }

    /** {@inheritDoc} */
    @Override
    public RealMatrix multiply(final RealMatrix m) {
        try {
            return multiply((OMRealMatrix) m);
        } catch (ClassCastException cce) {

            // safety check
            MatrixUtils.checkMultiplicationCompatible(this, m);

            final int outCols = m.getColumnDimension();
            final BlockRealMatrix out = new BlockRealMatrix(rows, outCols);
            for (OpenIntToDoubleHashMap.Iterator iterator = entries.iterator(); iterator.hasNext();) {
                iterator.advance();
                final double value = iterator.value();
                final int key      = iterator.key();
                final int i        = key / columns;
                final int k        = key % columns;
                for (int j = 0; j < outCols; ++j) {
                    out.addToEntry(i, j, value * m.getEntry(k, j));
                }
            }

            return out;
        }
    }
    public RealMatrix preMultiply(final RealMatrix m) {
    	RealMatrix   A = m;
    	OMRealMatrix B = this;
    	// safety check
    	MatrixUtils.checkMultiplicationCompatible(A,B);    	
    	final int outRows = A.getRowDimension();
    	final int outCols = B.getColumnDimension();
    	final BlockRealMatrix out = new BlockRealMatrix(outRows,outCols);
    	
    	//matrix multiplication
    	//C(a,b) = sum_i A(a,i) * B(i, b)
    	//so B(i,k) goes into C(*,k)
    	//C(*,k) += A(*,i) * B(i,k)
    	/*
    	 * [a b c]   [0]    [b]  C(1,0)
    	 * [d e f] * [1]  = [e]  C(2,0)
    	 *           [0]
    	 */
    	for (OpenIntToDoubleHashMap.Iterator iterator = entries.iterator(); iterator.hasNext();) {
    		iterator.advance();
    		final double value = iterator.value();
    		final int key      = iterator.key();
    		final int i        = key / columns; //row
    		final int k        = key % columns; //column
    		for (int j = 0; j < outRows; ++j) {
    			out.addToEntry(j,k, value * m.getEntry(j,i));
    		}
    	}

    	return out;

    }
    /**
     * Postmultiply this matrix by {@code m}.
     *
     * @param m Matrix to postmultiply by.
     * @return {@code this} * {@code m}.
     * @throws MatrixDimensionMismatchException
     * if the number of rows of {@code m} differ from the number of columns
     * of this matrix.
     */
    public OMRealMatrix multiply(OMRealMatrix m) {
    	MatrixUtils.checkMultiplicationCompatible(this, m);
    	int outCols = m.getColumnDimension();
    	OMRealMatrix out = new OMRealMatrix(rows, outCols);
        for (OpenIntToDoubleHashMap.Iterator iterator = entries.iterator(); iterator.hasNext();) {
            iterator.advance();
            final double value = iterator.value();
            final int key      = iterator.key();
            final int i = key / columns;
            final int k = key % columns;            	
            for (int j = 0; j < outCols; ++j) {
                final int rightKey = m.computeKey(k, j);
                if (m.entries.containsKey(rightKey)) {
                    final int outKey = out.computeKey(i, j);
                    final double outValue =
                        out.entries.get(outKey) + value * m.entries.get(rightKey);
                    if (outValue == 0.0) {
                        out.entries.remove(outKey);
                    } else {
                        out.entries.put(outKey, outValue);
                    }
                }
            }
        }

        return out;
    }
    
    @Override
    public OMRealMatrix transpose(){
    	OMRealMatrix out = new OMRealMatrix(this.getColumnDimension(),this.getRowDimension());
    	for (OpenIntToDoubleHashMap.Iterator iterator = entries.iterator(); iterator.hasNext();) {
            iterator.advance();
            final double value = iterator.value();
            final int key      = iterator.key();
            final int i = key / columns;
            final int k = key % columns;    
            out.setEntry(k, i, value);
    	}
    	return out;
    }
    
    /** {@inheritDoc} */
    @Override
    public double getEntry(int row, int column) {
        MatrixUtils.checkRowIndex(this, row);
        MatrixUtils.checkColumnIndex(this, column);
        return entries.get(computeKey(row, column));
    }

    /** {@inheritDoc} */
    @Override
    public int getRowDimension() {
        return rows;
    }

    /** {@inheritDoc} */
    @Override
    public void setEntry(int row, int column, double value) {
        MatrixUtils.checkRowIndex(this, row);
        MatrixUtils.checkColumnIndex(this, column);
        if (value == 0.0) {
            entries.remove(computeKey(row, column));
        } else {
            entries.put(computeKey(row, column), value);
        }
    }

    /** {@inheritDoc} */
    @Override
    public void addToEntry(int row, int column, double increment) {
        MatrixUtils.checkRowIndex(this, row);
        MatrixUtils.checkColumnIndex(this, column);
        final int key = computeKey(row, column);
        final double value = entries.get(key) + increment;
        if (value == 0.0) {
            entries.remove(key);
        } else {
            entries.put(key, value);
        }
    }

    /** {@inheritDoc} */
    @Override
    public void multiplyEntry(int row, int column, double factor) {
        MatrixUtils.checkRowIndex(this, row);
        MatrixUtils.checkColumnIndex(this, column);
        final int key = computeKey(row, column);
        final double value = entries.get(key) * factor;
        if (value == 0.0) {
            entries.remove(key);
        } else {
            entries.put(key, value);
        }
    }

    /**
     * Compute the key to access a matrix element
     * @param row row index of the matrix element
     * @param column column index of the matrix element
     * @return key within the map to access the matrix element
     */
    private int computeKey(int row, int column) {
        return row * columns + column;
    }


}
