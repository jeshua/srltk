/*******************************************************************************
 * Scala Reinforcement Learning Toolkit
 *   @author Jeshua Bratman
 *    @email jeshuabratman@gmail.com 
 * 
 * Copyright 2011 jeshua
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *   http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 ******************************************************************************/
package srltk.vis;
import java.awt.BorderLayout;
import java.awt.Color;

import javax.swing.JFrame;
import javax.swing.JPanel;

import org.jfree.chart.ChartFactory;
import org.jfree.chart.ChartPanel;
import org.jfree.chart.JFreeChart;
import org.jfree.chart.axis.CategoryAxis;
import org.jfree.chart.axis.CategoryLabelPositions;
import org.jfree.chart.axis.NumberAxis;
import org.jfree.chart.plot.CategoryPlot;
import org.jfree.chart.plot.PlotOrientation;
import org.jfree.chart.renderer.category.BarRenderer;
import org.jfree.data.category.CategoryDataset;
import org.jfree.data.category.DefaultCategoryDataset;

public class ActiveBarPlot extends JPanel{

	private static final long serialVersionUID = 1L;

	public static void main(final String[] args) {
		final ActiveBarPlot plot = new ActiveBarPlot();		
		plot.setVisible(true);	
		JFrame jf = new JFrame();
		jf.getContentPane().add(plot,BorderLayout.CENTER);
		jf.setSize(700,700);
		jf.setVisible(true);

		for(int j=0;j<30;j++)
			plot.setColor("first"+j, Color.black);
		plot.setColor("first15", Color.white);
		
		double v[] = new double[30];
		for(int i=0;i<1000;i++){
			for(int j=0;j<30;j++){
				plot.setValue(v[j],"first"+j);
				v[j] = Math.sin(0.3*(j+i));
			}			
			try {
				Thread.sleep(50);
			} catch (InterruptedException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}
		
	}
	
		
	private DefaultCategoryDataset dataset;
	public JFreeChart chart; 
	private BarRenderer renderer;
	final CategoryPlot plot;
	final ChartPanel chartPanel;
	
	public ActiveBarPlot(){ this("","","",false);}
	public ActiveBarPlot(final String title, String xlab, String ylab, Boolean legend) {
		dataset = new DefaultCategoryDataset();	
		
		// create the chart...
		chart = ChartFactory.createBarChart(
				title,         // chart title
				xlab,               // domain axis label
				ylab,                  // range axis label
				dataset,                  // data
				PlotOrientation.VERTICAL, // orientation
				legend,                     // include legend
				false,                     // tooltips?
				false                     // URLs?
		);
		plot = chart.getCategoryPlot();
		renderer = (BarRenderer) plot.getRenderer();
		 
		final JFreeChart chart = setupChart(dataset);
		chartPanel = new ChartPanel(chart);
		chartPanel.setPreferredSize(new java.awt.Dimension(450, 250));
		this.add(chartPanel);
		chartPanel.setVisible(true);		
	}

	public void setSize(int w, int h){
		super.setSize(w,h);
		chartPanel.setPreferredSize(new java.awt.Dimension(w, h));
	}
	
	public void setValue(double v, Comparable row){
		dataset.setValue(v,row,"");		
	}
	public void setColor(String row, Color color){		
		int n = dataset.getRowIndex(row);
		if(n <= 0) setValue(0,row);
		n = dataset.getRowIndex(row);
		renderer.setSeriesPaint(n, color);
	}
	public void removeValue(Comparable row){
		dataset.removeValue(row,"");
	}
	

	public void ylim(double min, double max){
		((NumberAxis) plot.getRangeAxis()).setRange(min,max);		
	}       
        
	
	
	private JFreeChart setupChart(final CategoryDataset dataset) {
		chart.setBackgroundPaint(Color.white);	
		plot.setBackgroundPaint(Color.lightGray);
		plot.setDomainGridlinePaint(Color.white);
		plot.setRangeGridlinePaint(Color.white);
		plot.setDomainCrosshairVisible(false);
		plot.setRangeCrosshairVisible(false);
		// set the range axis to display integers only...
		final NumberAxis rangeAxis = (NumberAxis) plot.getRangeAxis();
		rangeAxis.setStandardTickUnits(NumberAxis.createIntegerTickUnits());
		renderer.setDrawBarOutline(false);       
        
        
		
		final CategoryAxis domainAxis = plot.getDomainAxis();	
		domainAxis.setCategoryLabelPositions(
				CategoryLabelPositions.createUpRotationLabelPositions(Math.PI / 6.0)
		);
		return chart;

	}

}
