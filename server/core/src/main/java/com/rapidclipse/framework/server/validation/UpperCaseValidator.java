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
package com.rapidclipse.framework.server.validation;

import javax.validation.ConstraintValidator;
import javax.validation.ConstraintValidatorContext;

import com.rapidclipse.framework.server.validation.constraints.UpperCase;


/**
 * @author XDEV Software
 *
 */

public class UpperCaseValidator implements ConstraintValidator<UpperCase, CharSequence>
{
	/**
	 * {@inheritDoc}
	 */
	@Override
	public void initialize(final UpperCase constraintAnnotation)
	{
	}
	
	/**
	 * {@inheritDoc}
	 */
	@Override
	public boolean isValid(final CharSequence value, final ConstraintValidatorContext context)
	{
		if(value == null)
		{
			return true;
		}
		
		final String string = value.toString();
		return string.toUpperCase().equals(string);
	}
}
