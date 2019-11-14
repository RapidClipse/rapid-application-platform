
package com.rapidclipse.framework.server.charts.config;

import java.util.List;


public class XdevTrendlines
{
	
	private final List<Trendlines> lines;
	
	public XdevTrendlines(final List<Trendlines> lines)
	{
		this.lines = lines;
	}

	@Override
	public String toString()
	{
		final StringBuilder str = new StringBuilder();
		str.append("{ ");
		for(final Trendlines s : this.lines)
		{
			str.append(s.getRowNumber() + ": " + s + ",");

		}
		str.delete(str.length() - 1, str.length());
		str.append("}");

		return str.toString();
	}
}
