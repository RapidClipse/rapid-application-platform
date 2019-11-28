
package com.rapidclipse.framework.server.charts;

/**
 * @author XDEV Software
 *
 */
public interface HasAggregationTarget extends Chart
{
	public default AggregationTarget getAggregationTarget()
	{
		return properties().get("aggregationTarget");
	}

	public default void setAggregationTarget(final AggregationTarget aggregationTarget)
	{
		properties().put("aggregationTarget", aggregationTarget);
	}
}
