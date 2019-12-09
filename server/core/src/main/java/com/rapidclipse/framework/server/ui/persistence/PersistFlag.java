/*
 * Copyright (C) 2013-2019 by XDEV Software, All Rights Reserved.
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
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
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
