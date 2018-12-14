/*
 * Copyright (C) 2013-2018 by XDEV Software, All Rights Reserved.
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

package software.xdev.rap.server.ui.filter;


import java.util.Arrays;
import java.util.Objects;

import software.xdev.rap.server.data.ValueTransfer;


/**
 * @author XDEV Software
 *
 */
public final class FilterEntry
{
	private Object		propertyIdentifier;
	private String		operatorKey;
	private Object[]	values;
	
	
	public FilterEntry()
	{
	}
	
	
	public FilterEntry(final Object propertyIdentifier, final String operatorKey,
			final Object[] values)
	{
		setPropertyIdentifier(propertyIdentifier);
		setOperatorKey(operatorKey);
		setValues(values);
	}
	
	
	public void setPropertyIdentifier(final Object propertyIdentifier)
	{
		this.propertyIdentifier = ValueTransfer.put(propertyIdentifier);
	}
	
	
	public Object getPropertyIdentifier()
	{
		return ValueTransfer.get(this.propertyIdentifier);
	}
	
	
	public void setOperatorKey(final String operatorKey)
	{
		this.operatorKey = operatorKey;
	}
	
	
	public String getOperatorKey()
	{
		return this.operatorKey;
	}
	
	
	public void setValues(final Object[] values)
	{
		this.values = values == null ? null
				: Arrays.stream(values).map(ValueTransfer::put).toArray();
	}
	
	
	public Object[] getValues()
	{
		return this.values == null ? null
				: Arrays.stream(this.values).map(ValueTransfer::get).toArray();
	}


	@Override
	public boolean equals(final Object obj)
	{
		if(obj == this)
		{
			return true;
		}

		if(!(obj instanceof FilterEntry))
		{
			return false;
		}

		final FilterEntry other = (FilterEntry)obj;
		return Objects.equals(this.propertyIdentifier,other.propertyIdentifier)
				&& Objects.equals(this.operatorKey,other.operatorKey)
				&& Arrays.equals(this.values,other.values);
	}
}
