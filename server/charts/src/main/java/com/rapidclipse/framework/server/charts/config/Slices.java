
package com.rapidclipse.framework.server.charts.config;

import java.util.List;

import com.rapidclipse.framework.server.charts.pie.XdevPieSlice;


public class Slices
{
	private final List<XdevPieSlice> part;

	public Slices(final List<XdevPieSlice> part)
	{
		super();
		this.part = part;

	}
	
	@Override
	public String toString()
	{
		final StringBuilder str = new StringBuilder();
		str.append("{");
		int i = 0;
		for(final XdevPieSlice s : this.part)
		{
			str.append(i + ": {");

			if(s.getColor() != null)
			{
				str.append("color: '" + s.getColor() + "', ");
			}
			str.append("offset: " + s.getOffset() + ", textStyle: "
				+ s.getTextStyle() + "},");
			i++;
		}
		str.delete(str.length() - 1, str.length());
		str.append("}");
		
		return str.toString();
	}

}
