/*
 * RapidClipse Application Platform (RAP)
 *
 * Copyright (C) 2013-2024 by XDEV Software, All Rights Reserved.
 *
 * License: GNU Lesser General Public License (LGPL), version 3.0 or later.
 * See the LICENSE file in the root directory or https://www.gnu.org/licenses/lgpl+gpl-3.0.txt.
 */
package com.rapidclipse.framework.server.charts.maps;

import com.rapidclipse.framework.server.charts.AbstractChart;
import com.rapidclipse.framework.server.charts.JavaScriptable.ObjectHelper;


/**
 * @author XDEV Software
 * @since 10.02.00
 *
 */
public abstract class AbstractMapsChart extends AbstractChart
{
	private String mapsApiKey;
	
	protected AbstractMapsChart(final String type, final String... packages)
	{
		super(type, packages);
	}
	
	public String getMapsApiKey()
	{
		return this.mapsApiKey;
	}
	
	public void setMapsApiKey(final String mapsApiKey)
	{
		this.mapsApiKey = mapsApiKey;
	}
	
	@Override
	protected void createLoadOptions(final ObjectHelper obj)
	{
		super.createLoadOptions(obj);
		
		obj.putIfNotNull("mapsApiKey", this.mapsApiKey);
	}
}
