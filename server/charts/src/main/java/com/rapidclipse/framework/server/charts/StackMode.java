
package com.rapidclipse.framework.server.charts;

import com.rapidclipse.framework.server.util.JavaScriptable;


/**
 * @author XDEV Software
 * @since 10.02.00
 *
 */
public enum StackMode implements JavaScriptable
{
	FALSE("false"),
	TRUE("true"),
	PERCENT("'percent'"),
	RELATIVE("'relative'"),
	ABSOLUTE("'absolute'");
	
	private final String js;
	
	private StackMode(final String js)
	{
		this.js = js;
	}
	
	@Override
	public String js()
	{
		return this.js;
	}
}
