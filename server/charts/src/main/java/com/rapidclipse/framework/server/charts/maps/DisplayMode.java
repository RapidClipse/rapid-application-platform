
package com.rapidclipse.framework.server.charts.maps;

import com.rapidclipse.framework.server.util.JavaScriptable;

import elemental.json.Json;


/**
 * @author XDEV Software
 * @since 10.02.00
 *
 */
public enum DisplayMode implements JavaScriptable
{
	AUTO("auto"),
	REGIONS("regions"),
	MARKERS("markers"),
	TEXT("text");

	private final String js;

	private DisplayMode(final String js)
	{
		this.js = Json.create(js).toJson();
	}

	@Override
	public String js()
	{
		return this.js;
	}
}
