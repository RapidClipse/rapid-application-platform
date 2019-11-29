
package com.rapidclipse.framework.server.charts;

/**
 * @author XDEV Software
 * @since 10.02.00
 *
 */
public interface HasCandlestick extends Chart
{
	public default Candlestick getCandlestick()
	{
		return properties().get("candlestick");
	}
	
	public default void setCandlestick(final Candlestick candlestick)
	{
		properties().put("candlestick", candlestick);
	}
}
