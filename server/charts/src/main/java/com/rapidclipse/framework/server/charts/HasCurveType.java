
package com.rapidclipse.framework.server.charts;

/**
 * @author XDEV Software
 * @since 10.02.00
 *
 */
public interface HasCurveType extends Chart
{
	public default CurveType getCurveType()
	{
		return properties().get("curveType");
	}

	public default void setCurveType(final CurveType curveType)
	{
		properties().put("curveType", curveType);
	}
}
