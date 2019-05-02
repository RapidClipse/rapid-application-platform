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
import com.vaadin.flow.component.ComponentUtil;


/**
 * @author XDEV Software
 *
 */
public final class PersistFlag
{
	public static void set(final Component component, final boolean flag)
	{
		ComponentUtil.setData(component, PersistFlag.class, flag ? TRUE : FALSE);
	}
	
	public static boolean get(final Component component)
	{
		final PersistFlag flag = ComponentUtil.getData(component, PersistFlag.class);
		return flag != null ? flag.value : DEFAULT;
	}
	
	private final static boolean DEFAULT = false;
	
	private final static PersistFlag TRUE  = new PersistFlag(true);
	private final static PersistFlag FALSE = new PersistFlag(false);
	
	private final boolean value;
	
	private PersistFlag(final boolean value)
	{
		super();
		this.value = value;
	}
}
