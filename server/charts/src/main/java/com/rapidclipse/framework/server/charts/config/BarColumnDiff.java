
package com.rapidclipse.framework.server.charts.config;

public class BarColumnDiff
{
	private double widthFactor = 0.3;
	private String oldColor;

	public double getWidthFactor()
	{
		return this.widthFactor;
	}
	
	/**
	 * for Barchart or Columnchart
	 * Sets the relative width of the new data bar.
	 * 0.0-1.0
	 * Default: 0.3
	 *
	 * @param widthFactor
	 */
	public void setWidthFactor(final double widthFactor)
	{
		this.widthFactor = widthFactor;
	}
	
	public String getOldColor()
	{
		return this.oldColor;
	}
	
	public void setOldColor(final String oldColor)
	{
		this.oldColor = oldColor;
	}
	
	@Override
	public String toString()
	{
		final StringBuilder str = new StringBuilder();
		str.append("{ ");
		if(this.oldColor != null)
		{
			str.append("oldData: { color: '" + this.oldColor + "'}, ");
		}
		str.append("newData: { widthFactor: " + this.widthFactor + "} ");
		str.append("}");

		return str.toString();
	}
}
