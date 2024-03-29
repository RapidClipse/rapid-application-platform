/*
 * Copyright (C) 2013-2023 by XDEV Software, All Rights Reserved.
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
package com.rapidclipse.framework.server.reports;

import net.sf.jasperreports.engine.JRField;
import net.sf.jasperreports.engine.JRPropertiesHolder;
import net.sf.jasperreports.engine.JRPropertiesMap;
import net.sf.jasperreports.engine.JRPropertyExpression;


/**
 * @author XDEV Software
 *
 */
public abstract class DelegatingField implements JRField
{
	private final JRField delegate;
	
	public DelegatingField(final JRField delegate)
	{
		this.delegate = delegate;
	}
	
	@Override
	public String getDescription()
	{
		return this.delegate.getDescription();
	}
	
	@Override
	public String getName()
	{
		return this.delegate.getName();
	}
	
	@Override
	public Class<?> getValueClass()
	{
		return this.delegate.getValueClass();
	}
	
	@Override
	public String getValueClassName()
	{
		return this.delegate.getValueClassName();
	}
	
	@Override
	public void setDescription(final String arg0)
	{
		this.delegate.setDescription(arg0);
	}
	
	@Override
	public JRPropertiesHolder getParentProperties()
	{
		return this.delegate.getParentProperties();
	}
	
	@Override
	public JRPropertiesMap getPropertiesMap()
	{
		return this.delegate.getPropertiesMap();
	}
	
	@Override
	public boolean hasProperties()
	{
		return this.delegate.hasProperties();
	}
	
	@Override
	public JRPropertyExpression[] getPropertyExpressions()
	{
		return this.delegate.getPropertyExpressions();
	}
	
	@Override
	public Object clone()
	{
		return this.delegate.clone();
	}
}
