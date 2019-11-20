
package com.rapidclipse.framework.server.charts.calendar;

import java.util.HashMap;


public class XdevCalendarChartConfig
{
	private String   title;
	private Calendar calendar;

	public HashMap<String, Object> getOptions()
	{
		final HashMap<String, Object> options = new HashMap<>();
		options.put("calendar", this.calendar);
		options.put("title", this.title);
		return options;
	}
	
	public Calendar getCalendar()
	{
		return this.calendar;
	}
	
	public void setCalendar(final Calendar calendar)
	{
		this.calendar = calendar;
	}
	
	public String getTitle()
	{
		return this.title;
	}
	
	public void setTitle(final String title)
	{
		this.title = title;
	}
	
}
