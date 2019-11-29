
package com.rapidclipse.framework.server.charts;

/**
 * @author XDEV Software
 * @since 10.02.00
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
