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

package com.rapidclipse.framework.server.data.validator;

import java.net.MalformedURLException;
import java.net.URL;

import com.vaadin.flow.data.binder.ValidationResult;
import com.vaadin.flow.data.binder.ValueContext;
import com.vaadin.flow.data.validator.AbstractValidator;


/**
 *
 * @author XDEV Software
 *
 */
public class UrlValidator extends AbstractValidator<String>
{
	public UrlValidator(final String errorMessage)
	{
		super(errorMessage);
	}

	@Override
	public ValidationResult apply(final String value, final ValueContext context)
	{
		try
		{
			new URL(value);

			return ValidationResult.ok();
		}
		catch(final MalformedURLException e)
		{
			return ValidationResult.error(getMessage(value));
		}
	}
}
