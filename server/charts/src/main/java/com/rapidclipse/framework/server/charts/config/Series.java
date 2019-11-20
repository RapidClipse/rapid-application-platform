
package com.rapidclipse.framework.server.charts.config;

import java.util.List;


public class Series
{
	private final List<XdevSeries> lines;

	public Series(final List<XdevSeries> series)
	{
		super();
		this.lines = series;

	}
	
	@Override
	public String toString()
	{
		final StringBuilder str = new StringBuilder();
		str.append("{ ");
		for(final XdevSeries s : this.lines)
		{
			str.append(s.getNum() + ": " + s + ",");

		}
		str.delete(str.length() - 1, str.length());
		str.append("}");
		
		return str.toString();
	}

}
