
package com.rapidclipse.framework.server.charts;

/**
 * @author XDEV Software
 *
 */
public interface HasAxisTitlesPosition extends Chart
{
	public default AxisTitlesPosition getAxisTitlesPosition()
	{
		return properties().get("axisTitlesPosition");
	}
	
	public default void setAxisTitlesPosition(final AxisTitlesPosition axisTitlesPosition)
	{
		properties().put("axisTitlesPosition", axisTitlesPosition);
	}
}
