
package com.rapidclipse.framework.server.ui.filter;

import java.util.Arrays;
import java.util.Objects;


/**
 * @author XDEV Software
 *
 */
public final class FilterData
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
