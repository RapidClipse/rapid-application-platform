
package com.rapidclipse.framework.server.charts.combo;

import java.util.ArrayList;
import java.util.List;

import com.rapidclipse.framework.server.charts.config.XdevSeries;


public class Series
{
	private final List<String> type;

	public Series(final List<XdevSeries> series)
	{
		super();
		this.type = new ArrayList<>();
		for(final XdevSeries s : series)
		{
			this.type.add(s.getType());
		}

	}
	
	@Override
	public String toString()
	{
		final StringBuilder str = new StringBuilder();
		str.append("{");
		int i = 0;
		for(final String s : this.type)
		{
			str.append(i + ": {type: '" + s + "'},");
			i++;
		}
		str.delete(str.length() - 1, str.length());
		str.append("}");
		
		return str.toString();
	}

}
