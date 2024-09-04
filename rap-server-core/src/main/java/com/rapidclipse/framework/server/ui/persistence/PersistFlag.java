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
