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


/**
 * @author XDEV Software
 *
 */
public class MappedField extends DelegatingField
{
	private final String name;
	
	public MappedField(final JRField delegate, final String name)
	{
		super(delegate);
		
		this.name = name;
	}
	
	@Override
	public String getName()
	{
		return this.name;
	}
}
