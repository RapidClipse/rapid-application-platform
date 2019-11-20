
package com.rapidclipse.framework.server.charts.config;

import org.apache.commons.lang3.StringUtils;


public class Bar
{
	private String groupWidth = "61.8%";
	
	public Bar(final String groupWidth)
	{
		super();
		this.groupWidth = groupWidth;
	}
	
	public String getGroupWidth()
	{
		return this.groupWidth;
	}
	
	public void setGroupWidth(final String groupWidth)
	{
		this.groupWidth = groupWidth;
	}

	@Override
	public String toString()
	{
		final StringBuilder str = new StringBuilder();
		str.append("{");
		if(StringUtils.isNumeric(this.groupWidth))
		{
			str.append("groupWidth: " + this.groupWidth);
		}
		else
		{
			str.append("groupWidth: '" + this.groupWidth + "'");
		}
		
		str.append("}");

		return str.toString();
	}

}
