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
package com.rapidclipse.framework.server.ui.persistence;

import java.util.Collections;
import java.util.Map;
import java.util.stream.Collectors;


public final class GuiPersistenceEntry
{
	///////////////////////////////////////////////////////////////////////////
	// static methods //
	///////////////////
	
	public static GuiPersistenceEntry New(final Map<String, Object> values)
	{
		final GuiPersistenceEntry instance = new GuiPersistenceEntry();
		instance.values = Collections.unmodifiableMap(values);
		return instance;
	}
	
	///////////////////////////////////////////////////////////////////////////
	// instance fields //
	////////////////////
	
	Map<String, Object> values;
	
	///////////////////////////////////////////////////////////////////////////
	// constructors //
	/////////////////
	
	public GuiPersistenceEntry()
	{
		super();
	}
	
	///////////////////////////////////////////////////////////////////////////
	// override methods //
	/////////////////////
	
	public final synchronized Map<String, Object> values()
	{
		return this.values;
	}
	
	public Object value(final String key)
	{
		return this.values().get(key);
	}
	
	@Override
	public String toString()
	{
		return toString("");
	}
	
	String toString(final String prefix)
	{
		return this.values.entrySet().stream()
			.map(entry -> prefix + entry.getKey() + ":\t" + entry.getValue())
			.collect(Collectors.joining("\n"));
	}
	
}
