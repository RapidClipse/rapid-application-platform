/*
 * RapidClipse Application Platform (RAP)
 *
 * Copyright (C) 2013-2024 by XDEV Software, All Rights Reserved.
 *
 * License: GNU Lesser General Public License (LGPL), version 3.0 or later.
 * See the LICENSE file in the root directory or https://www.gnu.org/licenses/lgpl+gpl-3.0.txt.
 */
package com.rapidclipse.framework.server.ui.filter;

import java.io.Serializable;
import java.util.Arrays;
import java.util.Objects;


/**
 * @author XDEV Software
 *
 */
public final class FilterData implements Serializable
{
	private final String        searchTerm;
	private final FilterEntry[] entries;
	
	public FilterData()
	{
		this("", null);
	}
	
	public FilterData(final String searchTerm, final FilterEntry[] entries)
	{
		super();
		this.searchTerm = searchTerm;
		this.entries    = entries;
	}
	
	public String getSearchTerm()
	{
		return this.searchTerm;
	}
	
	public FilterEntry[] getEntries()
	{
		return this.entries;
	}
	
	@Override
	public boolean equals(final Object obj)
	{
		if(obj == this)
		{
			return true;
		}
		
		if(!(obj instanceof FilterData))
		{
			return false;
		}
		
		final FilterData other = (FilterData)obj;
		return Objects.equals(this.searchTerm, other.searchTerm)
			&& Arrays.equals(this.entries, other.entries);
	}
}
