
package com.rapidclipse.framework.server.charts;

/**
 * @author XDEV Software
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
