
package com.rapidclipse.framework.server.charts;

/**
 * @author XDEV Software
 *
 */
public interface HasFocusTarget extends Chart
{
	public default FocusTarget getFocusTarget()
	{
		return properties().get("focusTarget");
	}

	public default void setFocusTarget(final FocusTarget focusTarget)
	{
		properties().put("focusTarget", focusTarget);
	}
}
