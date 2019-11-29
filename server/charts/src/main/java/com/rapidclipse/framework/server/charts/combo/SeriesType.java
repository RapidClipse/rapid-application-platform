
package com.rapidclipse.framework.server.charts.combo;

import com.rapidclipse.framework.server.util.JavaScriptable;

import elemental.json.Json;


/**
 * @author XDEV Software
 * @since 10.02.00
 *
 */
public enum SeriesType implements JavaScriptable
{
	LINE("line"),
	AREA("area"),
	BARS("bars"),
	CANDLESTICKS("candlesticks"),
	STEPPED_AREA("steppedArea");

	private final String js;

	private SeriesType(final String js)
	{
		this.js = Json.create(js).toJson();
	}

	@Override
	public String js()
	{
		return this.js;
	}
}
