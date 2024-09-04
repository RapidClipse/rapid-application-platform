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
 * Validator for Inet Addresses, V4 and/or V6.
 *
 * @author XDEV Software
 *
 */
public class InetAddressValidator extends AbstractValidator<String>
{
	public static enum Protocol
	{
		IPV4()
		{
			@Override
			boolean validate(final String value)
			{
				return org.apache.commons.validator.routines.InetAddressValidator.getInstance()
					.isValidInet4Address(value);
			}
		},
		IPV6()
		{
			@Override
			boolean validate(final String value)
			{
				return org.apache.commons.validator.routines.InetAddressValidator.getInstance()
					.isValidInet6Address(value);
			}
		};
		
		abstract boolean validate(String value);
	}
	
	private final Protocol[] protocols;
	
	public InetAddressValidator(final String errorMessage)
	{
		this(errorMessage, Protocol.values());
	}
	
	public InetAddressValidator(final String errorMessage, final Protocol... protocols)
	{
		super(errorMessage);
		
		this.protocols = protocols;
	}
	
	@Override
	public ValidationResult apply(final String value, final ValueContext context)
	{
		for(final Protocol protocol : this.protocols)
		{
			if(protocol.validate(value))
			{
				return ValidationResult.ok();
			}
		}
		
		return ValidationResult.error(getMessage(value));
	}
}
