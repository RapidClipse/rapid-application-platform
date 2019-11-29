
package com.rapidclipse.framework.server.charts;

/**
 * @author XDEV Software
 * @since 10.02.00
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
