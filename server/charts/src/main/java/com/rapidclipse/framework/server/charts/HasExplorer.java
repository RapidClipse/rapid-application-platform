
package com.rapidclipse.framework.server.charts;

/**
 * @author XDEV Software
 * @since 10.02.00
 *
 */
public interface HasExplorer extends Chart
{
	public default Explorer getExplorer()
	{
		return properties().get("explorer");
	}

	public default void setExplorer(final Explorer explorer)
	{
		properties().put("explorer", explorer);
	}
}
