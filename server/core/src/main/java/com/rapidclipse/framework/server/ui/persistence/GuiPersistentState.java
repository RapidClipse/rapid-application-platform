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

import static java.util.Objects.requireNonNull;

import java.util.Collections;
import java.util.Map;


public final class GuiPersistentState
{
	///////////////////////////////////////////////////////////////////////////
	// static methods //
	///////////////////
	
	public static GuiPersistentState New(
		final String name,
		final Map<String, GuiPersistenceEntry> entries)
	{
		final GuiPersistentState instance = new GuiPersistentState();
		instance.name    = requireNonNull(name);
		instance.entries = Collections.unmodifiableMap(requireNonNull(entries));
		return instance;
	}
	
	///////////////////////////////////////////////////////////////////////////
	// instance fields //
	////////////////////
	
	String                           name;
	Map<String, GuiPersistenceEntry> entries;
	
	///////////////////////////////////////////////////////////////////////////
	// constructors //
	/////////////////
	
	GuiPersistentState()
	{
		super();
	}
	
	///////////////////////////////////////////////////////////////////////////
	// override methods //
	/////////////////////
	
	public final String name()
	{
		return this.name;
	}
	
	public final Map<String, GuiPersistenceEntry> entries()
	{
		return this.entries;
	}
	
	@Override
	public String toString()
	{
		final StringBuilder sb = new StringBuilder().append("------").append('\n').append(this.name)
			.append('\n').append("------").append('\n');
		this.entries.entrySet().forEach(e -> sb.append(e.getKey()).append(" = ")
			.append(e.getValue()).append('\n').append("---").append('\n'));
		return sb.toString();
	}
}
