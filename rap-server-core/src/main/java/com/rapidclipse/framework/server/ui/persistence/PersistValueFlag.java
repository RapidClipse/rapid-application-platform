/*
 * RapidClipse Application Platform (RAP)
 *
 * Copyright (C) 2013-2024 by XDEV Software, All Rights Reserved.
 *
 * License: GNU Lesser General Public License (LGPL), version 3.0 or later.
 * See the LICENSE file in the root directory or https://www.gnu.org/licenses/lgpl+gpl-3.0.txt.
 */
package com.rapidclipse.framework.server.ui.persistence;

import com.vaadin.flow.component.Component;
import com.vaadin.flow.component.ComponentUtil;


/**
 * @author XDEV Software
 *
 */
public final class PersistValueFlag
{
	public static void set(final Component component, final boolean flag)
	{
		ComponentUtil.setData(component, PersistValueFlag.class, flag ? TRUE : FALSE);
	}
	
	public static boolean get(final Component component)
	{
		final PersistValueFlag flag = ComponentUtil.getData(component, PersistValueFlag.class);
		return flag != null ? flag.value : DEFAULT;
	}
	
	private final static boolean DEFAULT = false;
	
	private final static PersistValueFlag TRUE  = new PersistValueFlag(true);
	private final static PersistValueFlag FALSE = new PersistValueFlag(false);
	
	private final boolean value;
	
	private PersistValueFlag(final boolean value)
	{
		super();
		this.value = value;
	}
}
