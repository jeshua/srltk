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

import java.awt.BasicStroke;
import java.awt.Color;
import java.util.ArrayList;
import java.text.*;
import javax.swing.JFrame;

import org.jfree.chart.ChartFactory;
import org.jfree.chart.ChartPanel;
import org.jfree.chart.JFreeChart;
import org.jfree.chart.plot.PlotOrientation;
import org.jfree.chart.plot.XYPlot;
import org.jfree.chart.renderer.category.LineAndShapeRenderer;
import org.jfree.chart.renderer.xy.XYItemRenderer;
import org.jfree.chart.renderer.xy.XYLineAndShapeRenderer;
import org.jfree.data.xy.XYSeries;
import org.jfree.data.xy.XYSeriesCollection;
import org.jfree.ui.ApplicationFrame;
import org.jfree.ui.RectangleInsets;
import org.jfree.chart.axis.*;

public class ActivePlot extends JFrame {

  private static final long serialVersionUID = 1L;

  private String title, xlab, ylab;
  private int width, height;
  private JFreeChart chart;
	
  private XYPlot plot; 
  private XYSeries dataset_series;
  private XYSeriesCollection dataset;
  private int series_index = -1;
	
  private boolean linesEnabled = true;
  private boolean markersEnabled = false;
  public void setLinesEnabled(boolean f) { linesEnabled = f;}
  public void setMarkersEnabled(boolean f) { markersEnabled = f;}
	
	
  //window average
  private int windowSize = 1;
  private ArrayList<Double> currentSums = new ArrayList<Double>();
  private ArrayList<Integer> currentPointCounts = new ArrayList<Integer>();
	

  public ActivePlot() {
    this("", "", "", 500, 400);
  }
  public ActivePlot(String title) {
    this(title, "", "", 500, 400);
  }
  public ActivePlot(String title, String xlab, String ylab) {
    this(title, xlab, ylab, 500, 400);
  }

  public ActivePlot(String title, String xlab, String ylab, int width,
                    int height) {
    super(title);
    this.title = title;
    this.xlab = xlab;
    this.ylab = ylab;
    this.width = width;
    this.height = height;
    init();
  }

  public void newDataset(String name) {
    dataset_series = new XYSeries(name);
    dataset.addSeries(dataset_series);
    currentSums.add(0d);
    currentPointCounts.add(0);
    this.series_index++;
		
		
    XYLineAndShapeRenderer renderer = (XYLineAndShapeRenderer) plot.getRenderer();
    renderer.setSeriesShapesVisible(series_index, markersEnabled);
		
		
    if(linesEnabled){
      renderer.setSeriesStroke(
                               series_index, new BasicStroke(
                                                             2.0f, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND,
                                                             1.0f)
                               );
    }else
      renderer.setSeriesLinesVisible(series_index, false);
      
		
    /*renderer.setSeriesStroke(
      series_index, new BasicStroke(
      2.0f, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND,
      1.0f, new float[] {10.0f, 6.0f}, 0.0f
      )
      );*/	
  }


  public void setYLog(){
    LogAxis rangeAxis =  new LogAxis(ylab);
    NumberFormat f = new DecimalFormat("0E0");
      rangeAxis.setNumberFormatOverride(f);
    plot.setRangeAxis(rangeAxis);
  }
  public void setXLog(){
    LogAxis rangeAxis =  new LogAxis(xlab);
    NumberFormat f = new DecimalFormat("0E0");
    rangeAxis.setNumberFormatOverride(f);
    plot.setDomainAxis(rangeAxis);
  }
  private void init() {

    // create dataset
    dataset = new XYSeriesCollection();
    // newDataset("");
    series_index = -1;

    // create chart
    chart = ChartFactory.createXYLineChart(title, xlab, ylab, dataset,
                                           PlotOrientation.VERTICAL, true, true, false);

    chart.setBackgroundPaint(Color.white);

    plot   = (XYPlot) chart.getPlot();
    
  

    plot.setBackgroundPaint(Color.lightGray);
    plot.setDomainGridlinePaint(Color.white);
    plot.setRangeGridlinePaint(Color.white);
    plot.setAxisOffset(new RectangleInsets(5.0, 5.0, 5.0, 5.0));
    plot.setDomainCrosshairVisible(true);
    plot.setRangeCrosshairVisible(true);

    XYItemRenderer r = plot.getRenderer();
    if (r instanceof XYLineAndShapeRenderer) {
      XYLineAndShapeRenderer renderer = (XYLineAndShapeRenderer) r;
      renderer.setBaseShapesVisible(true);
      renderer.setBaseShapesFilled(true);
      renderer.setDrawSeriesLineAsPath(true);
    }

    // create panel
    ChartPanel panel = new ChartPanel(this.chart);
    panel.setFillZoomRectangle(true);
    // panel.setMouseWheelEnabled(true);
    panel.setPreferredSize(new java.awt.Dimension(width, height));
    setContentPane(panel);

  }

  public void addPoint(int x, double y) {
    if (series_index < 0)
      newDataset("");
		
    addPointToSeries(x, y, series_index);
  }

  public void addPointToSeries(int x, double y, int series) {
    while (series > series_index)
      newDataset("");
    currentSums.set(series,currentSums.get(series)+y);
    currentPointCounts.set(series,currentPointCounts.get(series)+1);
    if(currentPointCounts.get(series) >= windowSize){
      XYSeries s = dataset.getSeries(series);
      s.add(x, currentSums.get(series)/windowSize);
      currentSums.set(series,0d);
      currentPointCounts.set(series,0);
    }
		
  }
	
	
  public void setWindowedAverage(int windowSize){
    this.windowSize = windowSize;
  }
	

  public void display() {
    this.pack();
    this.setVisible(true);
  }
}
