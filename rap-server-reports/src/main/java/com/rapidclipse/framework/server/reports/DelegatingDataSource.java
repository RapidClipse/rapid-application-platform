/*
 * RapidClipse Application Platform (RAP)
 *
 * Copyright (C) 2013-2024 by XDEV Software, All Rights Reserved.
 *
 * License: GNU Lesser General Public License (LGPL), version 3.0 or later.
 * See the LICENSE file in the root directory or https://www.gnu.org/licenses/lgpl+gpl-3.0.txt.
 */
package com.rapidclipse.framework.server.reports;

import net.sf.jasperreports.engine.JRDataSource;
import net.sf.jasperreports.engine.JRException;
import net.sf.jasperreports.engine.JRField;


/**
 * @author XDEV Software
 *
 */
public class DelegatingDataSource implements JRDataSource
{
	private final JRDataSource delegate;
	
	public DelegatingDataSource(final JRDataSource delegate)
	{
		this.delegate = delegate;
	}
	
	@Override
	public boolean next() throws JRException
	{
		return this.delegate.next();
	}
	
	@Override
	public Object getFieldValue(final JRField jrField) throws JRException
	{
		return this.delegate.getFieldValue(jrField);
	}
}
