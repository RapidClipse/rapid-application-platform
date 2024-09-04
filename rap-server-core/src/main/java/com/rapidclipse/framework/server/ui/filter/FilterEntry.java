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

import com.rapidclipse.framework.server.data.ValueTransfer;


/**
 * @author XDEV Software
 *
 */
public final class FilterEntry implements Serializable
{
	private Object   propertyIdentifier;
	private String   operatorKey;
	private Object[] values;
	
	public FilterEntry()
	{
	}
	
	public FilterEntry(
		final Object propertyIdentifier,
		final String operatorKey,
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
		return Objects.equals(this.propertyIdentifier, other.propertyIdentifier)
			&& Objects.equals(this.operatorKey, other.operatorKey)
			&& Arrays.equals(this.values, other.values);
	}
}
