
package com.rapidclipse.framework.server.charts.org;

import com.rapidclipse.framework.server.util.JavaScriptable;

import elemental.json.Json;


/**
 * @author XDEV Software
 * @since 10.02.00
 *
 */
public enum Size implements JavaScriptable
{
	SMALL("small"),
	MEDIUM("medium"),
	LARGE("large");

	private final String js;

	private Size(final String js)
	{
		this.js = Json.create(js).toJson();
	}

	@Override
	public String js()
	{
		return this.js;
	}
}
