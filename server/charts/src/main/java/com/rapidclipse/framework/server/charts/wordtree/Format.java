
package com.rapidclipse.framework.server.charts.wordtree;

import com.rapidclipse.framework.server.util.JavaScriptable;

import elemental.json.Json;


/**
 * @author XDEV Software
 * @since 10.02.00
 *
 */
public enum Format implements JavaScriptable
{
	IMPLICIT("implicit"),
	EXPLICIT("explicit");

	private final String js;

	private Format(final String js)
	{
		this.js = Json.create(js).toJson();
	}

	@Override
	public String js()
	{
		return this.js;
	}
}
