/*  Experimental
 * only working for gauge changeValue() yet *
 */

package com.rapidclipse.framework.server.charts.config;

public class Animation
{
	private Integer duration = 200;
	private boolean startup  = true;
	private String  easing   = "linear";

	@Override
	public String toString()
	{
		final StringBuilder str = new StringBuilder();
		str.append("{");
		str.append("duration: " + this.duration + ", ");
		str.append("easing: '" + this.easing + "', ");
		str.append("startup: " + this.startup);
		str.append("}");
		return str.toString();
	}

	public Integer getDuration()
	{
		return this.duration;
	}

	public boolean isStartup()
	{
		return this.startup;
	}

	public String getEasing()
	{
		return this.easing;
	}
	
	public void setDuration(final Integer duration)
	{
		this.duration = duration;
	}
	
	public void setStartup(final boolean startup)
	{
		this.startup = startup;
	}
	
	public void setEasing(final String easing)
	{
		this.easing = easing;
	}
	
}
