
package com.rapidclipse.framework.server.charts;

/**
 * @author XDEV Software
 * @since 10.02.00
 *
 */
public interface CanInterpolateNulls extends Chart
{
	public default Boolean getInterpolateNulls()
	{
		return properties().get("interpolateNulls");
	}
	
	public default void setInterpolateNulls(final Boolean interpolateNulls)
	{
		properties().put("interpolateNulls", interpolateNulls);
	}
}
