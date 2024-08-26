/*
 * RapidClipse Application Platform (RAP)
 *
 * Copyright (C) 2013-2024 by XDEV Software, All Rights Reserved.
 *
 * License: GNU Lesser General Public License (LGPL), version 3.0 or later.
 * See the LICENSE file in the root directory or https://www.gnu.org/licenses/lgpl+gpl-3.0.txt.
 */
package com.rapidclipse.framework.server.ui.persistence;

import java.util.Collections;
import java.util.Map;


public final class GuiPersistentStates
{
	///////////////////////////////////////////////////////////////////////////
	// static methods //
	///////////////////
	
	public static GuiPersistentStates New(final Map<String, GuiPersistentState> states)
	{
		final GuiPersistentStates instance = new GuiPersistentStates();
		instance.states = Collections.unmodifiableMap(states);
		return instance;
	}
	
	///////////////////////////////////////////////////////////////////////////
	// instance fields //
	////////////////////
	
	Map<String, GuiPersistentState> states;
	
	///////////////////////////////////////////////////////////////////////////
	// constructors //
	/////////////////
	
	GuiPersistentStates()
	{
		super();
	}
	
	///////////////////////////////////////////////////////////////////////////
	// declared methods //
	/////////////////////
	
	public synchronized Map<String, GuiPersistentState> states()
	{
		return this.states;
	}
	
	///////////////////////////////////////////////////////////////////////////
	// override methods //
	/////////////////////
	
	@Override
	public String toString()
	{
		final StringBuilder vs = new StringBuilder();
		for(final GuiPersistentState e : this.states.values())
		{
			vs.append(e).append("\n\n");
		}
		return vs.toString();
	}
	
}
