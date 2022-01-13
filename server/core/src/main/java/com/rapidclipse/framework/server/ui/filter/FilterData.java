/*
 * Copyright (C) 2013-2022 by XDEV Software, All Rights Reserved.
 *
 * This file is part of the RapidClipse Application Platform (RAP).
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *
 * Contributors:
 *     XDEV Software - initial API and implementation
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
