
package com.rapidclipse.framework.server.charts;

/**
 * @author XDEV Software
 * @since 10.02.00
 *
 */
public interface HasInteractivity extends Chart
{
	public default Boolean getEnableInteractivity()
	{
		return properties().get("enableInteractivity");
	}

	public default void setEnableInteractivity(final Boolean enableInteractivity)
	{
		properties().put("enableInteractivity", enableInteractivity);
	}
}
