
package com.rapidclipse.framework.server.charts;

/**
 * @author XDEV Software
 *
 */
public interface Stackable extends Chart
{
	public default Boolean isStacked()
	{
		return properties().get("isStacked");
	}

	public default void setStacked(final Boolean stacked)
	{
		properties().put("isStacked", stacked);
	}
}
