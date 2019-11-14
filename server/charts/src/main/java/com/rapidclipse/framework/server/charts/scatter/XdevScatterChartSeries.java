
package com.rapidclipse.framework.server.charts.scatter;

import java.io.Serializable;

import com.rapidclipse.framework.server.charts.config.Options;


public class XdevScatterChartSeries implements Serializable
{
	private String  color;
	private String  labelInLegend;
	private boolean visibleInLegend = true;
	private Integer lineWidth       = 0;
	private Integer pointSize       = 8;
	private String  pointShape      = Options.POINTSHAPE_CIRCLE;

	/**
	 * @return the color
	 */
	public String getColor()
	{
		return this.color;
	}

	/**
	 * The color to use for this series. Specify a valid HTML color string. <br>
	 *
	 * @param color
	 *            the color to set
	 */
	public void setColor(final String color)
	{
		this.color = color;
	}

	/**
	 * @return the labelInLegend
	 */
	public String getLabelInLegend()
	{
		return this.labelInLegend;
	}

	/**
	 * The description of the series to appear in the chart legend. <br>
	 *
	 * @param labelInLegend
	 *            the labelInLegend to set
	 */
	public void setLabelInLegend(final String labelInLegend)
	{
		this.labelInLegend = labelInLegend;
	}

	/**
	 * @return the visibleInLegend
	 */
	public boolean isVisibleInLegend()
	{
		return this.visibleInLegend;
	}

	/**
	 * A boolean value, where true means that the series should have a legend entry,
	 * and false means that it should not. Default is true. <br>
	 *
	 * @param visibleInLegend
	 *            the visibleInLegend to set
	 */
	public void setVisibleInLegend(final boolean visibleInLegend)
	{
		this.visibleInLegend = visibleInLegend;
	}

	/**
	 * @return the lineWidth
	 */
	public Integer getLineWidth()
	{
		return this.lineWidth;
	}

	/**
	 * Data line width in pixels. Use zero to hide all lines and show only the
	 * points. <br>
	 *
	 * @param lineWidth
	 *            the lineWidth to set
	 */
	public void setLineWidth(final Integer lineWidth)
	{
		this.lineWidth = lineWidth;
	}

	/**
	 * @return the pointSize
	 */
	public Integer getPointSize()
	{
		return this.pointSize;
	}

	/**
	 * Diameter of displayed points in pixels. Use zero to hide all points. <br>
	 *
	 * @param pointSize
	 *            the pointSize to set
	 */
	public void setPointSize(final Integer pointSize)
	{
		this.pointSize = pointSize;
	}

	public String getPointShape()
	{
		return this.pointShape;
	}

	/**
	 * The shape of individual data elements: 'circle', 'triangle', 'square',
	 * 'diamond', 'star', or 'polygon'. <br>
	 *
	 * @param pointShape
	 */
	public void setPointShape(final String pointShape)
	{
		this.pointShape = pointShape;
	}

	@Override
	public String toString()
	{
		final StringBuilder str = new StringBuilder();
		str.append("{");
		if(this.color != null)
		{
			str.append("color: '" + this.color + "', ");
		}
		str.append("labelInLegend: '" + this.labelInLegend + "', ");
		str.append("visibleInLegend: " + this.visibleInLegend + ", ");
		str.append("lineWidth: " + this.lineWidth + ", ");
		str.append("pointSize: " + this.pointSize + ", ");
		str.append("pointShape: '" + this.pointShape + "' ");
		str.append("}");

		return str.toString();
	}
	
}
