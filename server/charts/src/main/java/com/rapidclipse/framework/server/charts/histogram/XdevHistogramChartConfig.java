
package com.rapidclipse.framework.server.charts.histogram;

import java.io.Serializable;
import java.util.HashMap;

import com.rapidclipse.framework.server.charts.AbstractXdevChartConfig;


public class XdevHistogramChartConfig extends AbstractXdevChartConfig implements Serializable
{
	private Histogram histogram;

	@Override
	public HashMap<String, Object> getOptions()
	{
		final HashMap<String, Object> options = super.getOptions();
		options.put("histogram", this.histogram);
		
		return options;
	}
	
	public Histogram getHistogram()
	{
		return this.histogram;
	}
	
	public void setHistogram(final Histogram histogram)
	{
		this.histogram = histogram;
	}
	
}
