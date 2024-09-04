/*
 * RapidClipse Application Platform (RAP)
 *
 * Copyright (C) 2013-2024 by XDEV Software, All Rights Reserved.
 *
 * License: GNU Lesser General Public License (LGPL), version 3.0 or later.
 * See the LICENSE file in the root directory or https://www.gnu.org/licenses/lgpl+gpl-3.0.txt.
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
