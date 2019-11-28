
package com.rapidclipse.framework.server.charts;

/**
 * @author XDEV Software
 *
 */
public interface HasCategories extends Chart
{
	public default Boolean getReverseCategories()
	{
		return properties().get("reverseCategories");
	}

	public default void setReverseCategories(final Boolean reverseCategories)
	{
		properties().put("reverseCategories", reverseCategories);
	}
}
