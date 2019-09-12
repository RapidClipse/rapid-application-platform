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
package com.rapidclipse.framework.server.data.validator;

import java.util.Arrays;

import com.vaadin.flow.data.binder.ValidationResult;
import com.vaadin.flow.data.binder.ValueContext;
import com.vaadin.flow.data.validator.AbstractValidator;


/**
 * @author XDEV Software
 *
 */
public class CreditCardValidator extends AbstractValidator<String>
{
	public static enum Issuer
	{
		AMEX(org.apache.commons.validator.routines.CreditCardValidator.AMEX),
		VISA(org.apache.commons.validator.routines.CreditCardValidator.VISA),
		MASTERCARD(org.apache.commons.validator.routines.CreditCardValidator.MASTERCARD),
		DISCOVER(org.apache.commons.validator.routines.CreditCardValidator.DISCOVER),
		DINERS(org.apache.commons.validator.routines.CreditCardValidator.DINERS),
		VPAY(org.apache.commons.validator.routines.CreditCardValidator.VPAY);
		
		private final long mask;
		
		private Issuer(final long mask)
		{
			this.mask = mask;
		}
	}
	
	private final org.apache.commons.validator.routines.CreditCardValidator validator;
	
	/**
	 * Creates a credit card validator, either a generic one, or one with all issuers.
	 *
	 * @param errorMessage
	 * @param generic
	 *            <code>true</code> to create a generic one which doesn't check the Issuer Identification Number (IIN),
	 *            <code>false</code> to create it with all {@link Issuer}s supported.
	 */
	public CreditCardValidator(final String errorMessage, final boolean generic)
	{
		super(errorMessage);
		
		if(generic)
		{
			this.validator = org.apache.commons.validator.routines.CreditCardValidator.genericCreditCardValidator();
		}
		else
		{
			this.validator = new org.apache.commons.validator.routines.CreditCardValidator(
				getIssuersMask(Issuer.values()));
		}
	}
	
	public CreditCardValidator(final String errorMessage, final Issuer... issuers)
	{
		super(errorMessage);
		
		this.validator = new org.apache.commons.validator.routines.CreditCardValidator(
			getIssuersMask(issuers));
	}
	
	private long getIssuersMask(final Issuer... issuer)
	{
		return Arrays.stream(issuer).mapToLong(i -> i.mask).sum();
	}
	
	@Override
	public ValidationResult apply(final String value, final ValueContext context)
	{
		if(this.validator.isValid(value))
		{
			return ValidationResult.ok();
		}
		
		return ValidationResult.error(getMessage(value));
	}
}
