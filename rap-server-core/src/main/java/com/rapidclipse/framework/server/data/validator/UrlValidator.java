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
