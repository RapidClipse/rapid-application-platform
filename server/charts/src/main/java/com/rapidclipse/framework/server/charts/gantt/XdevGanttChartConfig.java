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

package com.rapidclipse.framework.server.charts.gantt;

import java.io.Serializable;
import java.util.HashMap;


/**
 *
 * @author XDEV Software (SS)
 * @since 4.0
 */
public class XdevGanttChartConfig implements Serializable
{
	
	private Gantt gantt = new Gantt();
	
	public HashMap<String, Object> getOptions()
	{
		final HashMap<String, Object> options = new HashMap<>();
		options.put("gantt", this.gantt);
		return options;
	}
	
	public Gantt getGantt()
	{
		return this.gantt;
	}
	
	public void setGantt(final Gantt gantt)
	{
		this.gantt = gantt;
	}
}
