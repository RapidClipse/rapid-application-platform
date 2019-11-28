
package com.rapidclipse.framework.server.charts;

/**
 * @author XDEV Software
 *
 */
public interface HasSelectionMode extends Chart
{
	public default SelectionMode getSelectionMode()
	{
		return properties().get("selectionMode");
	}

	public default void setSelectionMode(final SelectionMode selectionMode)
	{
		properties().put("selectionMode", selectionMode);
	}
}
