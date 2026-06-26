/*
 * RapidClipse Application Platform (RAP)
 *
 * Copyright (C) 2013-2024 by XDEV Software, All Rights Reserved.
 *
 * License: GNU Lesser General Public License (LGPL), version 3.0 or later.
 * See the LICENSE file in the root directory or https://www.gnu.org/licenses/lgpl+gpl-3.0.txt.
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
