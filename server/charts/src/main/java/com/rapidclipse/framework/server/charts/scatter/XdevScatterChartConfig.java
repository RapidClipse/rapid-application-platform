
package com.rapidclipse.framework.server.charts.scatter;

import java.io.Serializable;
import java.util.HashMap;

import com.rapidclipse.framework.server.charts.AbstractXdevChartConfig;
import com.rapidclipse.framework.server.charts.config.HAxis;
import com.rapidclipse.framework.server.charts.config.Series;
import com.rapidclipse.framework.server.charts.config.VAxis;


public class XdevScatterChartConfig extends AbstractXdevChartConfig implements Serializable
{
	
	private HAxis  hAxis;
	private VAxis  vAxis;
	private Series series;
	
	@Override
	public HashMap<String, Object> getOptions()
	{
		final HashMap<String, Object> options = super.getOptions();
		options.put("hAxis", this.hAxis);
		options.put("vAxis", this.vAxis);
		options.put("series", this.series);
		
		return options;
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
