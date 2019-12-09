/*
 * Copyright (C) 2013-2019 by XDEV Software, All Rights Reserved.
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
