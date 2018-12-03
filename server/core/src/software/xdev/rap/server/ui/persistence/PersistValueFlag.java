/*
 * Copyright (C) 2013-2018 by XDEV Software, All Rights Reserved.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 *
 * For further information see
 * <http://www.rapidclipse.com/en/legal/license/license.html>.
 */

package software.xdev.rap.server.ui.persistence;


import com.vaadin.flow.component.Component;
import com.vaadin.flow.component.ComponentUtil;


/**
 * @author XDEV Software
 *
 */
public class PersistValueFlag
{
	public static void set(final Component component, final boolean flag)
	{
		ComponentUtil.setData(component,PersistValueFlag.class,flag ? TRUE : FALSE);
	}
	
	
	public static boolean get(final Component component)
	{
		final PersistValueFlag flag = ComponentUtil.getData(component,PersistValueFlag.class);
		return flag != null ? flag.value : DEFAULT;
	}

	private final static boolean			DEFAULT	= false;
	
	private final static PersistValueFlag	TRUE	= new PersistValueFlag(true);
	private final static PersistValueFlag	FALSE	= new PersistValueFlag(false);
	
	private final boolean					value;
	
	
	private PersistValueFlag(final boolean value)
	{
		super();
		this.value = value;
	}
}
