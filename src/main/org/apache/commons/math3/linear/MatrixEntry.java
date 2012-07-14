package org.apache.commons.math3.linear;

public class MatrixEntry{
	public int row;
	public int col;
	public double value;  
	public MatrixEntry(int r, int c, double v){this.row = r;this.col = c;this.value = v;}
}