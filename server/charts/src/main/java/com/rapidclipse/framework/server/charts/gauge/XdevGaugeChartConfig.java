
package com.rapidclipse.framework.server.charts.gauge;

import java.util.HashMap;

import com.rapidclipse.framework.server.charts.config.Animation;


public class XdevGaugeChartConfig
{

	private Integer   greenFrom;
	private Integer   greenTo;
	private Integer   max        = 100;
	private Integer   min        = 0;
	private Integer   redFrom    = 85;
	private Integer   redTo      = 100;
	private Integer   yellowFrom = 60;
	private Integer   yellowTo   = 85;
	private Animation animation  = new Animation();

	public HashMap<String, Object> getOptions()
	{
		final HashMap<String, Object> options = new HashMap<>();
		options.put("greenFrom", this.greenFrom);
		options.put("greenTo", this.greenTo);
		options.put("max", this.max);
		options.put("min", this.min);
		options.put("redFrom", this.redFrom);
		options.put("redTo", this.redTo);
		options.put("yellowFrom", this.yellowFrom);
		options.put("yellowTo", this.yellowTo);
		options.put("animation", this.animation);
		return options;
	}
	
	public Integer getGreenFrom()
	{
		return this.greenFrom;
	}
	
	public void setGreenFrom(final Integer greenFrom)
	{
		this.greenFrom = greenFrom;
	}
	
	public Integer getGreenTo()
	{
		return this.greenTo;
	}
	
	public void setGreenTo(final Integer greenTo)
	{
		this.greenTo = greenTo;
	}
	
	public Integer getMax()
	{
		return this.max;
	}
	
	public void setMax(final Integer max)
	{
		this.max = max;
	}
	
	public Integer getMin()
	{
		return this.min;
	}
	
	public void setMin(final Integer min)
	{
		this.min = min;
	}
	
	public Integer getRedFrom()
	{
		return this.redFrom;
	}
	
	public void setRedFrom(final Integer redFrom)
	{
		this.redFrom = redFrom;
	}
	
	public Integer getRedTo()
	{
		return this.redTo;
	}
	
	public void setRedTo(final Integer redTo)
	{
		this.redTo = redTo;
	}
	
	public Integer getYellowFrom()
	{
		return this.yellowFrom;
	}
	
	public void setYellowFrom(final Integer yellowFrom)
	{
		this.yellowFrom = yellowFrom;
	}
	
	public Integer getYellowTo()
	{
		return this.yellowTo;
	}
	
	public void setYellowTo(final Integer yellowTo)
	{
		this.yellowTo = yellowTo;
	}
	
	public Animation getAnimation()
	{
		return this.animation;
	}
	
	public void setAnimation(final Animation animation)
	{
		this.animation = animation;
	}
	
}
