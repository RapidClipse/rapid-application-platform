/*
 * Copyright (C) 2013-2019 by XDEV Software, All Rights Reserved.
 *
 * This file is part of the RapidClipse Application Platform (RAP).
 *
 * RAP is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * RAP is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with RAP. If not, see <http://www.gnu.org/licenses/>.
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *
 * Contributors:
 *     XDEV Software Corp. - initial API and implementation
 */
package com.rapidclipse.framework.server.charts.org;

import java.io.Serializable;
import java.util.HashMap;

import com.rapidclipse.framework.server.charts.config.Options;


/**
 *
 * @author XDEV Software
 * @since 10.02.00
 */
public class XdevOrgChartConfig implements Serializable
{
	private boolean allowCollapse = false;
	private boolean allowHtml     = false;
	private String  size          = Options.SIZE_MEDIUM;

	public HashMap<String, Object> getOptions()
	{
		final HashMap<String, Object> options = new HashMap<>();
		options.put("allowCollapse", this.allowCollapse);
		options.put("allowHtml", this.allowHtml);
		options.put("size", this.size);
		return options;
	}
	
	public boolean isAllowCollapse()
	{
		return this.allowCollapse;
	}
	
	public void setAllowCollapse(final boolean allowCollapse)
	{
		this.allowCollapse = allowCollapse;
	}
	
	public boolean isAllowHtml()
	{
		return this.allowHtml;
	}
	
	public void setAllowHtml(final boolean allowHtml)
	{
		this.allowHtml = allowHtml;
	}
	
	public String getSize()
	{
		return this.size;
	}

	/**
	 * Size of the nodes
	 *
	 * @param size
	 *            'small', 'medium' or 'large'
	 */
	public void setSize(final String size)
	{
		this.size = size;
	}
}
