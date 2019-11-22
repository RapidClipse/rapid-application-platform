
package com.rapidclipse.framework.server.charts;

import static java.util.Objects.requireNonNull;

import com.rapidclipse.framework.server.util.JavaScriptable.ObjectHelper;


/**
 * @author XDEV Software
 *
 */
public abstract class MapsApiChart extends Chart
{
	private String apiKey = "";

	protected MapsApiChart(final String type, final String... packages)
	{
		super(type, packages);
	}

	/**
	 * @param apiKey
	 *            the apiKey to set
	 */
	public void setApiKey(final String apiKey)
	{
		this.apiKey = requireNonNull(apiKey);
	}

	/**
	 * @return the apiKey
	 */
	public String getApiKey()
	{
		return this.apiKey;
	}

	@Override
	protected void createLoadOptions(final ObjectHelper obj)
	{
		super.createLoadOptions(obj);

		obj.putIfNotNull("mapsApiKey", this.apiKey);
	}
}
