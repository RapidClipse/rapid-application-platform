
package com.rapidclipse.framework.server.charts.config;

import java.util.List;


public class Series
{
	private final List<Object> lines;

	public Series(final List<Object> series)
	{
		super();
		this.lines = series;

	}
	
	@Override
	public String toString()
	{
		final StringBuilder str = new StringBuilder();
		str.append("{");
		int i = 0;
		for(final Object s : this.lines)
		{
			str.append(i + ":  " + s + ",");
			i++;
		}
		str.delete(str.length() - 1, str.length());
		str.append("}");
		
		return str.toString();
	}

}
