/*
 * RapidClipse Application Platform (RAP)
 *
 * Copyright (C) 2013-2024 by XDEV Software, All Rights Reserved.
 *
 * License: GNU Lesser General Public License (LGPL), version 3.0 or later.
 * See the LICENSE file in the root directory or https://www.gnu.org/licenses/lgpl+gpl-3.0.txt.
 */
package com.rapidclipse.framework.server.data.validator;

import com.vaadin.flow.data.binder.ValidationResult;
import com.vaadin.flow.data.binder.ValueContext;
import com.vaadin.flow.data.validator.AbstractValidator;


/**
 * <p>
 * Validator for Internet domain names and top-level domains.
 * </p>
 *
 * <p>
 * Domain names are evaluated according
 * to the standards <a href="https://www.ietf.org/rfc/rfc1034.txt">RFC1034</a>,
 * section 3, and <a href="https://www.ietf.org/rfc/rfc1123.txt">RFC1123</a>,
 * section 2.1. No accommodation is provided for the specialized needs of
 * other applications; if the domain name has been URL-encoded, for example,
 * validation will fail even though the equivalent plaintext version of the
 * same name would have passed.
 * </p>
 *
 * <p>
 * Validation is also provided for top-level domains (TLDs) as defined and
 * maintained by the Internet Assigned Numbers Authority (IANA):
 * </p>
 *
 * @author XDEV Software
 *
 */
public class DomainValidator extends AbstractValidator<String>
{
	public DomainValidator(final String errorMessage)
	{
		super(errorMessage);
	}

	@Override
	public ValidationResult apply(final String value, final ValueContext context)
	{
		if(org.apache.commons.validator.routines.DomainValidator.getInstance().isValid(value))
		{
			return ValidationResult.ok();
		}

		return ValidationResult.error(getMessage(value));
	}
}
