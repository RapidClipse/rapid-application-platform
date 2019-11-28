
package com.rapidclipse.framework.server.charts;

/**
 * @author XDEV Software
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
