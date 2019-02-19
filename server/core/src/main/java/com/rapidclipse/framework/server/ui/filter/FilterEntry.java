/*-
 * ---
 * Rapid Application Platform / Server / Core
 * --
 * Copyright (C) 2013 - 2019 XDEV Software Corp.
 * --
 * This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License 2.0
 * which accompanies this distribution, and is available at
 * https://www.eclipse.org/legal/epl-2.0/
 * 
 * SPDX-License-Identifier: EPL-2.0
 * 
 * Contributors:
 *     XDEV Software Corp. - initial API and implementation
 * ---
 */

package com.rapidclipse.framework.server.ui.filter;

import java.util.Arrays;
import java.util.Objects;

import com.rapidclipse.framework.server.data.ValueTransfer;


/**
 * @author XDEV Software
 *
 */
public final class FilterEntry
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
