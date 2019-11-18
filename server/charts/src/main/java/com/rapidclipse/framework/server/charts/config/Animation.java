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

	/**
	 * The duration of the animation, in milliseconds.
	 *
	 * @param duration
	 */
	public void setDuration(final Integer duration)
	{
		this.duration = duration;
	}

	/**
	 * Determines if the chart will animate on the initial draw. If true, the chart will start at the baseline and
	 * animate to its final state.
	 *
	 * @param startup
	 */
	public void setStartup(final boolean startup)
	{
		this.startup = startup;
	}

	/**
	 * *
	 * <li>'linear' - Constant speed.</li>
	 * <li>'in' - Ease in - Start slow and speed up.</li>
	 * <li>'out' - Ease out - Start fast and slow down.</li>
	 * <li>'inAndOut' - Ease in and out - Start slow, speed up, then slow down.</li>
	 *
	 * @param easing
	 */
	public void setEasing(final String easing)
	{
		this.easing = easing;
	}

}
