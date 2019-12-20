
package com.rapidclipse.framework.server.webapi;

import java.io.Serializable;


/**
 *
 * @author XDEV Software
 * @since 10.02.00
 */
public class JavascriptError implements Serializable
{
	private final String message;
	private final String name;
	
	public JavascriptError(final String message, final String name)
	{
		this.message = message;
		this.name    = name;
	}
	
	public String getMessage()
	{
		return this.message;
	}
	
	public String getName()
	{
		return this.name;
	}
}
