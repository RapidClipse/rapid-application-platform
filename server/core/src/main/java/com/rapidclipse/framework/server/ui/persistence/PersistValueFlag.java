/*
 * Copyright (C) 2013-2022 by XDEV Software, All Rights Reserved.
 *
 * This file is part of the RapidClipse Application Platform (RAP).
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *
 * Contributors:
 *     XDEV Software - initial API and implementation
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
