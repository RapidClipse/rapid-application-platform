
package com.rapidclipse.framework.server.charts;

/**
 * @author XDEV Software
 * @since 10.02.00
 *
 */
public interface HasCrosshair extends Chart
{
	public default Crosshair getCrosshair()
	{
		return properties().get("crosshair");
	}

	public default void setCrosshair(final Crosshair crosshair)
	{
		properties().put("crosshair", crosshair);
	}
}
