
package com.rapidclipse.framework.server.charts;

/**
 * @author XDEV Software
 * @since 10.02.00
 *
 */
public interface DiffChart extends Chart
{
	public void setModel(ChartModel before, ChartModel after);
}
