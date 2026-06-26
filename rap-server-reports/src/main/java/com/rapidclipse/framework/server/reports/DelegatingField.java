/*
 * RapidClipse Application Platform (RAP)
 *
 * Copyright (C) 2013-2024 by XDEV Software, All Rights Reserved.
 *
 * License: GNU Lesser General Public License (LGPL), version 3.0 or later.
 * See the LICENSE file in the root directory or https://www.gnu.org/licenses/lgpl+gpl-3.0.txt.
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
