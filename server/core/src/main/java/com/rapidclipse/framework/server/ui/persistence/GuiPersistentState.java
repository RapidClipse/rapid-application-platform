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
