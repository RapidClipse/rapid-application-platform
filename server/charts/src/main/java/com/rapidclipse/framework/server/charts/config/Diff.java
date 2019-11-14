
package com.rapidclipse.framework.server.charts.config;

public class Diff
{
	private double  radiusFactor = 0.5;
	private double  borderFactor = 0.02;
	private double  oldOpacity   = 0.5;
	private double  newOpacity   = 1.0;
	private boolean inCenter     = true;

	public double getRadiusFactor()
	{
		return this.radiusFactor;
	}

	/**
	 * !!PIE CHART!!
	 * How big the inner circle should be displayed.
	 *
	 * @param radiusFactor
	 */
	public void setRadiusFactor(final double radiusFactor)
	{
		this.radiusFactor = radiusFactor;
	}
	
	public double getBorderFactor()
	{
		return this.borderFactor;
	}
	
	/**
	 * Thickness of the border.
	 *
	 * @param borderFactor
	 */
	public void setBorderFactor(final double borderFactor)
	{
		this.borderFactor = borderFactor;
	}
	
	public double getOldOpacity()
	{
		return this.oldOpacity;
	}
	
	/**
	 * Opacity of the old data.
	 *
	 * @param oldOpacity
	 */
	public void setOldOpacity(final double oldOpacity)
	{
		this.oldOpacity = oldOpacity;
	}
	
	public double getNewOpacity()
	{
		return this.newOpacity;
	}
	
	/**
	 * Opacity of the new data.
	 *
	 * @param newOpacity
	 */
	public void setNewOpacity(final double newOpacity)
	{
		this.newOpacity = newOpacity;
	}
	
	public boolean isInCenter()
	{
		return this.inCenter;
	}
	
	/**
	 * !!PIE CHART!!
	 * Switch the old data to the outside.
	 *
	 * @param inCenter
	 */
	public void setInCenter(final boolean inCenter)
	{
		this.inCenter = inCenter;
	}

	@Override
	public String toString()
	{
		final StringBuilder str = new StringBuilder();
		str.append("{ ");
		str.append("innerCircle: { radiusFactor: " + this.radiusFactor + ", ");
		str.append("borderFactor: " + this.borderFactor + "}, ");
		str.append("oldData: { opacity: " + this.oldOpacity + ", ");
		str.append("inCenter: " + this.inCenter + "}, ");
		str.append("newData: { opacity: " + this.newOpacity + "} ");

		str.append("}");

		return str.toString();
	}

}
