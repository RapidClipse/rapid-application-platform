
package com.rapidclipse.framework.server.charts.candlestick;

import com.rapidclipse.framework.server.charts.BackgroundStyle;


public class Candlestick
{
	private BackgroundStyle fallingColor;
	private BackgroundStyle risingColor;
	
	public BackgroundStyle getFallingColor()
	{
		return this.fallingColor;
	}
	
	public void setFallingColor(final BackgroundStyle fallingColor)
	{
		this.fallingColor = fallingColor;
	}
	
	public BackgroundStyle getRisingColor()
	{
		return this.risingColor;
	}
	
	public void setRisingColor(final BackgroundStyle risingColor)
	{
		this.risingColor = risingColor;
	}
	
	@Override
	public String toString()
	{
		final StringBuilder str = new StringBuilder();

		str.append("{ fallingColor: " + this.fallingColor + ", ");
		str.append("risingColor: " + this.risingColor + "} ");
		
		return str.toString();
	}
}
