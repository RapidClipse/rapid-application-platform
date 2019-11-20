
package com.rapidclipse.framework.server.charts.config;

import java.util.List;


public class IntervalGlobal
{
	private String               style     = "line";
	private Integer              pointSize = 2;
	private double               lineWidth = 0.5;
	private double               barWidth  = 0.5;
	private String               color;
	private double               boxWidth  = 0.5;
	private final List<Interval> inters;

	/**
	 * Set a List of Intervals (or null) for global settings of intervals
	 *
	 * @param inters
	 */
	public IntervalGlobal(final List<Interval> inters)
	{
		this.inters = inters;
	}
	
	public String getStyle()
	{
		return this.style;
	}
	
	/**
	 * 'line', 'bar', 'box', 'stick', 'point' and 'area'.
	 *
	 * @param style
	 */
	public void setStyle(final String style)
	{
		this.style = style;
	}
	
	public Integer getPointSize()
	{
		return this.pointSize;
	}
	
	public void setPointSize(final Integer pointSize)
	{
		this.pointSize = pointSize;
	}
	
	public double getLineWidth()
	{
		return this.lineWidth;
	}
	
	public void setLineWidth(final double lineWidth)
	{
		this.lineWidth = lineWidth;
	}
	
	public double getBarWidth()
	{
		return this.barWidth;
	}
	
	public void setBarWidth(final double barWidth)
	{
		this.barWidth = barWidth;
	}
	
	public String getColor()
	{
		return this.color;
	}
	
	public void setColor(final String color)
	{
		this.color = color;
	}
	
	public double getBoxWidth()
	{
		return this.boxWidth;
	}
	
	public void setBoxWidth(final double boxWidth)
	{
		this.boxWidth = boxWidth;
	}

	@Override
	public String toString()
	{
		final StringBuilder str = new StringBuilder();
		str.append("{");
		str.append("boxWidth: " + this.boxWidth + ", ");
		str.append("pointSize: " + this.pointSize + ", ");
		str.append("lineWidth: " + this.lineWidth + ", ");
		str.append("barWidth: " + this.barWidth + ", ");
		if(this.color != null)
		{
			str.append("color: '" + this.color + "', ");
		}
		str.append("'style': '" + this.style + "' ");
		str.append("}");
		if(this.inters != null)
		{
			str.append(", interval: { ");
			for(final Interval i : this.inters)
			{
				str.append("'" + i.getName() + "': " + i + ",");
			}
			str.delete(str.length() - 1, str.length());
			str.append("}");
		}
		return str.toString();
	}
	
}
