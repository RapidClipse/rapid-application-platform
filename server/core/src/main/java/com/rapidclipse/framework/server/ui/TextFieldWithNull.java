/*
 * Copyright (C) 2013-2019 by XDEV Software, All Rights Reserved.
 *
 * This file is part of the RapidClipse Application Platform (RAP).
 *
 * RAP is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * RAP is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with RAP. If not, see <http://www.gnu.org/licenses/>.
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *
 * Contributors:
 *     XDEV Software Corp. - initial API and implementation
 */
package com.rapidclipse.framework.server.ui;

import com.vaadin.flow.component.textfield.TextField;


/**
 * A {@link TextField} which interprets <code>null</code> as an empty String and
 * doesn't throw an Exception.
 *
 * @author XDEV Software
 *
 */
public class TextFieldWithNull extends TextField
{
	/**
	 *
	 */
	public TextFieldWithNull()
	{
		super();
	}
	
	/**
	 * @param label
	 * @param initialValue
	 * @param placeholder
	 */
	public TextFieldWithNull(
		final String label,
		final String initialValue,
		final String placeholder)
	{
		super(label, initialValue, placeholder);
	}
	
	/**
	 * @param label
	 * @param initialValue
	 * @param listener
	 */
	public TextFieldWithNull(
		final String label,
		final String initialValue,
		final ValueChangeListener<? super ComponentValueChangeEvent<TextField, String>> listener)
	{
		super(label, initialValue, listener);
	}
	
	/**
	 * @param label
	 * @param placeholder
	 */
	public TextFieldWithNull(final String label, final String placeholder)
	{
		super(label, placeholder);
	}
	
	/**
	 * @param label
	 * @param listener
	 */
	public TextFieldWithNull(
		final String label,
		final ValueChangeListener<? super ComponentValueChangeEvent<TextField, String>> listener)
	{
		super(label, listener);
	}
	
	/**
	 * @param label
	 */
	public TextFieldWithNull(final String label)
	{
		super(label);
	}
	
	/**
	 * @param listener
	 */
	public TextFieldWithNull(
		final ValueChangeListener<? super ComponentValueChangeEvent<TextField, String>> listener)
	{
		super(listener);
	}
	
	@Override
	public void setValue(final String value)
	{
		super.setValue(value == null ? "" : value);
	}
}
