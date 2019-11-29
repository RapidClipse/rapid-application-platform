
package com.rapidclipse.framework.server.charts;

/**
 * @author XDEV Software
 * @since 10.02.00
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
