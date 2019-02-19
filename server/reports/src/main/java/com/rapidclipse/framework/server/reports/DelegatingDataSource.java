/*-
 * ---
 * Rapid Application Platform / Server / Reports
 * --
 * Copyright (C) 2013 - 2019 XDEV Software Corp.
 * --
 * This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License 2.0
 * which accompanies this distribution, and is available at
 * https://www.eclipse.org/legal/epl-2.0/
 * 
 * SPDX-License-Identifier: EPL-2.0
 * 
 * Contributors:
 *     XDEV Software Corp. - initial API and implementation
 * ---
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
