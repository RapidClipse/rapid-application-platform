
package com.rapidclipse.framework.server.charts;

/**
 * @author XDEV Software
 *
 */
public interface AllowsIFrame extends Chart
{
	public default Boolean getForceIFrame()
	{
		return properties().get("forceIFrame");
	}

	public default void setForceIFrame(final Boolean forceIFrame)
	{
		properties().put("forceIFrame", forceIFrame);
	}
}
