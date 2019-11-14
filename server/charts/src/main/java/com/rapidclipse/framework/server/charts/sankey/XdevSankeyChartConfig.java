
package com.rapidclipse.framework.server.charts.sankey;

import java.util.HashMap;


public class XdevSankeyChartConfig
{
	private Sankey sankey;
	
	public HashMap<String, Object> getOptions()
	{
		final HashMap<String, Object> options = new HashMap<>();
		options.put("sankey", this.sankey);
		return options;
	}
	
	public Sankey getSankey()
	{
		return this.sankey;
	}
	
	public void setSankey(final Sankey sankey)
	{
		this.sankey = sankey;
	}
	
}
