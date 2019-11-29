
package com.rapidclipse.framework.server.charts;

/**
 * @author XDEV Software
 *
 */
public interface DiffChart extends Chart
{
	public void setModel(ChartModel before, ChartModel after);
}
