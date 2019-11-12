/*
 * Copyright (C) 2013-2019 by XDEV Software, All Rights Reserved.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 *
 * For further information see
 * <http://www.rapidclipse.com/en/legal/license/license.html>.
 */

package com.rapidclipse.framework.server.charts.combo;

import java.util.List;

import com.rapidclipse.framework.server.charts.config.XdevSeries;
import com.rapidclipse.framework.server.charts.data.DataTable;


/**
 * @author XDEV Software
 *
 */
public class ComboChartComponentState
{
	private XdevComboChartConfig config;
	private DataTable            dataTable;
	private List<XdevSeries>     series;

	public XdevComboChartConfig getConfig()
	{
		return this.config;
	}

	public void setConfig(final XdevComboChartConfig config)
	{
		this.config = config;
	}

	public DataTable getDataTable()
	{
		return this.dataTable;
	}

	public void setDataTable(final DataTable dataTable)
	{
		this.dataTable = dataTable;
	}

	/**
	 * @return the series
	 */
	public List<XdevSeries> getSeries()
	{
		return this.series;
	}

	/**
	 * @param series
	 *            the series to set
	 */
	public void setSeries(final List<XdevSeries> series)
	{
		this.series = series;
	}
	
}
