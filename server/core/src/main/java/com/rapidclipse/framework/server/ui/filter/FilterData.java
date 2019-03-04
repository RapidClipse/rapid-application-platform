/*
 * Copyright (C) 2013-2019 by XDEV Software, All Rights Reserved.
 *
 * This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License 2.0
 * which accompanies this distribution, and is available at
 * https://www.eclipse.org/legal/epl-2.0/
 *
 * SPDX-License-Identifier: EPL-2.0
 *
 * Contributors:
 *     XDEV Software Corp. - initial API and implementation
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
