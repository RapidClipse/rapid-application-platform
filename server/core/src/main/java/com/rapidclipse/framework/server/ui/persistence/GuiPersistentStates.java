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
