
package com.rapidclipse.framework.server.charts;

/**
 * @author XDEV Software
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
