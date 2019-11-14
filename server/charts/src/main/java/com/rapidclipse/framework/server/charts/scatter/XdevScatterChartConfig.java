
package com.rapidclipse.framework.server.charts.scatter;

import java.io.Serializable;
import java.util.HashMap;

import com.rapidclipse.framework.server.charts.AbstractXdevChartConfig;
import com.rapidclipse.framework.server.charts.config.HAxis;
import com.rapidclipse.framework.server.charts.config.Series;
import com.rapidclipse.framework.server.charts.config.VAxis;
import com.rapidclipse.framework.server.charts.config.XdevTrendlines;


public class XdevScatterChartConfig extends AbstractXdevChartConfig implements Serializable
{

	private HAxis          hAxis;
	private VAxis          vAxis;
	private Series         series;
	private XdevTrendlines trendlines;

	@Override
	public HashMap<String, Object> getOptions()
	{
		final HashMap<String, Object> options = super.getOptions();
		options.put("hAxis", this.hAxis);
		options.put("vAxis", this.vAxis);
		options.put("series", this.series);
		options.put("trendlines", this.trendlines);
		
		return options;
	}
	
	public XdevTrendlines getTrendline()
	{
		return this.trendlines;
	}
	
	/**
	 * A trendline is a line superimposed on a chart revealing the overall direction of the data.
	 *
	 * @param trendline
	 */
	public void setTrendline(final XdevTrendlines trendline)
	{
		this.trendlines = trendline;
	}
	
	public HAxis gethAxis()
	{
		return this.hAxis;
	}
	
	public void sethAxis(final HAxis hAxis)
	{
		this.hAxis = hAxis;
	}
	
	public VAxis getvAxis()
	{
		return this.vAxis;
	}
	
	public void setvAxis(final VAxis vAxis)
	{
		this.vAxis = vAxis;
	}

	public Series getSeries()
	{
		return this.series;
	}

	public void setSeries(final Series series)
	{
		this.series = series;
	}

}
