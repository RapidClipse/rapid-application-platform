
package com.rapidclipse.framework.server.charts.config;

public abstract class XdevSeries
{
	private Integer num;

	public XdevSeries(final int num)
	{
		this.num = num;
	}
	
	public Integer getNum()
	{
		return this.num;
	}

	public void setNum(final Integer num)
	{
		this.num = num;
	}

}
