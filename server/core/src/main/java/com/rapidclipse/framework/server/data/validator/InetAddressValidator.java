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
