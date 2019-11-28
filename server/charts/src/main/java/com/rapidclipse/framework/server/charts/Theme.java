
package com.rapidclipse.framework.server.charts;

import com.rapidclipse.framework.server.util.JavaScriptable;


/**
 * @author XDEV Software
 *
 */
public enum Theme implements JavaScriptable
{
	NULL("null"),
	MAXIMIZED("'maximized'");
	
	private final String js;
	
	private Theme(final String js)
	{
		this.js = js;
	}
	
	@Override
	public String js()
	{
		return this.js;
	}
}
