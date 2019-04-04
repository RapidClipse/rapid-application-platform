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
