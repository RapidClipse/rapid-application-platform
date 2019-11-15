
package com.rapidclipse.framework.server.charts.config;

public class Interval
{
	private String  name;
	private String  style;
	private Integer fillOpacity;
	private String  color;
	private Integer pointSize;
	private Integer lineWidth;
	private String  curveType;

	public Interval(final String name)
	{
		this.name = name;
	}
	
	public String getName()
	{
		return this.name;
	}
	
	public void setName(final String name)
	{
		this.name = name;
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

	public Integer getFillOpacity()
	{
		return this.fillOpacity;
	}

	public void setFillOpacity(final Integer fillOpacity)
	{
		this.fillOpacity = fillOpacity;
	}

	public String getColor()
	{
		return this.color;
	}

	public void setColor(final String color)
	{
		this.color = color;
	}

	public Integer getPointSize()
	{
		return this.pointSize;
	}

	public void setPointSize(final Integer pointSize)
	{
		this.pointSize = pointSize;
	}

	public Integer getLineWidth()
	{
		return this.lineWidth;
	}

	public void setLineWidth(final Integer lineWidth)
	{
		this.lineWidth = lineWidth;
	}

	public String getCurveType()
	{
		return this.curveType;
	}

	public void setCurveType(final String curveType)
	{
		this.curveType = curveType;
	}

	@Override
	public String toString()
	{
		final StringBuilder str = new StringBuilder();
		str.append("{");
		str.append("fillOpacity: " + this.fillOpacity + ", ");
		str.append("pointSize: " + this.pointSize + ", ");
		str.append("lineWidth: " + this.lineWidth + ", ");
		str.append("curveType: '" + this.curveType + "', ");
		if(this.color != null)
		{
			str.append("color: '" + this.color + "', ");
		}
		str.append("style: '" + this.style + "' ");
		str.append("}");
		return str.toString();
	}
}
