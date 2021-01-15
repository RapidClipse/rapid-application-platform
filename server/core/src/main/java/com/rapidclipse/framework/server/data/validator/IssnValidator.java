/*
 * Copyright (C) 2013-2021 by XDEV Software, All Rights Reserved.
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
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *
 * Contributors:
 *     XDEV Software Corp. - initial API and implementation
 */
package com.rapidclipse.framework.server.data.validator;

import com.vaadin.flow.data.binder.ValidationResult;
import com.vaadin.flow.data.binder.ValueContext;
import com.vaadin.flow.data.validator.AbstractValidator;


/**
 * ISSN Validator.
 * <p>
 * International Standard Serial Number (ISSN)
 * is an eight-digit serial number used to
 * uniquely identify a serial publication.
 * 
 * <pre>
 * The format is:
 *
 * ISSN dddd-dddC
 * where:
 * d = decimal digit (0-9)
 * C = checksum (0-9 or X)
 *
 * The checksum is formed by adding the first 7 digits multiplied by
 * the position in the entire number (counting from the right).
 *
 * For example, abcd-efg would be 8a + 7b + 6c + 5d + 4e +3f +2g.
 * The check digit is modulus 11, where the value 10 is represented by 'X'
 * For example:
 * ISSN 0317-8471
 * ISSN 1050-124X
 *
 * @author XDEV Software
 *
 */
public class IssnValidator extends AbstractValidator<String>
{
	public IssnValidator(final String errorMessage)
	{
		super(errorMessage);
	}

	@Override
	public ValidationResult apply(final String value, final ValueContext context)
	{
		if(org.apache.commons.validator.routines.ISSNValidator.getInstance().isValid(value))
		{
			return ValidationResult.ok();
		}
		
		return ValidationResult.error(getMessage(value));
	}
}
