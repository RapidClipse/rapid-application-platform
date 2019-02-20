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

import com.vaadin.flow.component.Component;


/**
 * Helper class to quickly save and restore GUI states.
 *
 * @see #save(Component, String)
 * @see #load(Component, String, String)
 * @see #persist(Component, String)
 * @see #restore(Component, String, GuiPersistentStates)
 *
 * @author XDEV Software
 */
public final class GuiPersistence
{
	private GuiPersistence()
	{
	}
	
	/**
	 * Persists the state of <code>component</code> and its sub-components and
	 * returns the serialized data.
	 *
	 * @param component
	 *            the root component
	 * @param name
	 *            the state's name
	 * @return the serialized state data
	 * @see #load(Component, String, String)
	 */
	public static String save(final Component component, final String name)
	{
		return GuiPersistenceSerializer.DEFAULT.serialize(persist(component, name));
	}
	
	/**
	 * Persists the state of <code>component</code> and its sub-components.
	 *
	 * @param component
	 *            the root component
	 * @param name
	 *            the state's name
	 * @return the state data
	 * @see #restore(Component, String, GuiPersistentStates)
	 */
	public static GuiPersistentStates persist(final Component component, final String name)
	{
		return GuiPersistenceAnalyzer.New().createPersister(name, component).persistState();
	}
	
	/**
	 * Restores a previously saved state of the <code>component</code> and its
	 * sub-components.
	 *
	 * @param component
	 *            the root component
	 * @param name
	 *            the state's name
	 * @param data
	 *            the serialized data
	 * @see #save(Component, String)
	 */
	public static void load(final Component component, final String name, final String data)
	{
		restore(component, name, GuiPersistenceSerializer.DEFAULT.deserialize(data));
	}
	
	/**
	 * Restores a previously saved state of the <code>component</code> and its
	 * sub-components.
	 *
	 * @param component
	 *            the root component
	 * @param name
	 *            the state's name
	 * @param states
	 *            the persistence state
	 * @see #persist(Component, String)
	 */
	public static void restore(
		final Component component,
		final String name,
		final GuiPersistentStates states)
	{
		GuiPersistenceAnalyzer.New().createPersister(name, component).restoreState(states);
	}
}
