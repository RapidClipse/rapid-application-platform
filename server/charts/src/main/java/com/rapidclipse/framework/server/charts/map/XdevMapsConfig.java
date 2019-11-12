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

package com.rapidclipse.framework.server.charts.map;

import java.io.Serializable;
import java.util.HashMap;


/**
 *
 * @author XDEV Software (SS)
 * @since 4.0
 */
public class XdevMapsConfig implements Serializable
{
	private boolean showTooltip    = true;
	private boolean showInfoWindow = true;
	
	public HashMap<String, Object> getOptions()
	{
		final HashMap<String, Object> options = new HashMap<>();
		options.put("showTooltip", this.showTooltip);
		options.put("showInfoWindow", this.showInfoWindow);
		return options;
	}
	
	public boolean isShowTooltip()
	{
		return this.showTooltip;
	}
	
	public void setShowTooltip(final boolean showTooltip)
	{
		this.showTooltip = showTooltip;
	}
	
	public boolean isShowInfoWindow()
	{
		return this.showInfoWindow;
	}
	
	public void setShowInfoWindow(final boolean showInfoWindow)
	{
		this.showInfoWindow = showInfoWindow;
	}
	
}
