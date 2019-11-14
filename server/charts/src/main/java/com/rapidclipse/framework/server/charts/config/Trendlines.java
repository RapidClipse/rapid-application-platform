
package com.rapidclipse.framework.server.charts.config;

public class Trendlines
{
	private int     rowNumber;
	private String  type          = "linear";
	private String  color;
	private double  opacity       = 0.6;
	private double  degree        = 3;
	private Integer lineWidth     = 2;
	private Integer pointSize     = 10;
	private boolean showR2;
	private boolean visibleInLegend;
	private String  labelInLegend = "";
	
	public Trendlines(final int rowNumber)
	{
		this.rowNumber = rowNumber;
	}
	
	public String getType()
	{
		return this.type;
	}
	
	public int getRowNumber()
	{
		return this.rowNumber;
	}
	
	public void setRowNumber(final int rowNumber)
	{
		this.rowNumber = rowNumber;
	}
	
	/**
	 * Sets the type of the trendline
	 * <li>'linear'</li>
	 * <li>'exponential'</li>
	 * <li>'polynomial'</li>
	 *
	 * @param type
	 *            linear, exponential, polynomial
	 */
	public void setType(final String type)
	{
		this.type = type;
	}
	
	public boolean isVisibleInLegend()
	{
		return this.visibleInLegend;
	}
	
	public void setVisibleInLegend(final boolean visibleInLegend)
	{
		this.visibleInLegend = visibleInLegend;
	}
	
	public String getColor()
	{
		return this.color;
	}
	
	public void setColor(final String color)
	{
		this.color = color;
	}
	
	public double getDegree()
	{
		return this.degree;
	}
	
	public void setDegree(final double degree)
	{
		this.degree = degree;
	}
	
	public Integer getLineWidth()
	{
		return this.lineWidth;
	}
	
	public void setLineWidth(final Integer lineWidth)
	{
		this.lineWidth = lineWidth;
	}
	
	public double getOpacity()
	{
		return this.opacity;
	}
	
	public void setOpacity(final double opacity)
	{
		this.opacity = opacity;
	}
	
	public Integer getPointSize()
	{
		return this.pointSize;
	}
	
	/**
	 * Set the size of the trendline dots.
	 *
	 * @param pointSize
	 */
	public void setPointSize(final Integer pointSize)
	{
		this.pointSize = pointSize;
	}
	
	public String getLabelInLegend()
	{
		return this.labelInLegend;
	}
	
	public void setLabelInLegend(final String labelInLegend)
	{
		this.labelInLegend = labelInLegend;
	}
	
	public boolean isShowR2()
	{
		return this.showR2;
	}
	
	public void setShowR2(final boolean showR2)
	{
		this.showR2 = showR2;
	}
	
	@Override
	public String toString()
	{
		final StringBuilder str = new StringBuilder();
		str.append("{");
		str.append("type: '" + this.type + "', ");
		str.append("opacity: " + this.opacity + ", ");
		if(this.color != null)
		{
			str.append("color: '" + this.color + "', ");
		}
		str.append("labelInLegend: '" + this.labelInLegend + "', ");
		str.append("lineWidth: " + this.lineWidth + ", ");
		str.append("pointSize: " + this.pointSize + ", ");
		str.append("showR2: " + this.showR2 + ", ");
		str.append("degree: " + this.degree + ", ");
		str.append("visibleInLegend: " + this.visibleInLegend + " ");
		str.append("}");
		return str.toString();
	}
}
