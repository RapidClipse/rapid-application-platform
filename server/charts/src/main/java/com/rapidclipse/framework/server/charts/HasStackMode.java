
package com.rapidclipse.framework.server.charts;

/**
 * @author XDEV Software
 *
 */
public interface HasStackMode extends Chart
{
	public default StackMode getStackMode()
	{
		return properties().get("isStacked");
	}

	public default void setStackMode(final StackMode stackMode)
	{
		properties().put("isStacked", stackMode);
	}
}
