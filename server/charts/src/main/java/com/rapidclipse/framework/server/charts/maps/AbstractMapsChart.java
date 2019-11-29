
package com.rapidclipse.framework.server.charts.maps;

import com.rapidclipse.framework.server.charts.ChartBase;
import com.rapidclipse.framework.server.util.JavaScriptable.ObjectHelper;


/**
 * @author XDEV Software
 * @since 10.02.00
 *
 */
public abstract class AbstractMapsChart extends ChartBase
{
	private String mapsApiKey;
	
	protected AbstractMapsChart(final String type, final String... packages)
	{
		super(type, packages);
	}
	
	public String getMapsApiKey()
	{
		return this.mapsApiKey;
	}
	
	public void setMapsApiKey(final String mapsApiKey)
	{
		this.mapsApiKey = mapsApiKey;
	}
	
	@Override
	protected void createLoadOptions(final ObjectHelper obj)
	{
		super.createLoadOptions(obj);
		
		obj.putIfNotNull("mapsApiKey", this.mapsApiKey);
	}
}
