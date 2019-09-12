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
package com.rapidclipse.framework.server.data.converter;

import static java.util.Objects.requireNonNull;

import java.nio.charset.StandardCharsets;
import java.util.function.Supplier;

import com.rapidclipse.framework.security.util.PasswordHasher;
import com.vaadin.flow.data.binder.Result;
import com.vaadin.flow.data.binder.ValueContext;
import com.vaadin.flow.data.converter.Converter;


/**
 * @author XDEV Software
 */
public interface StringToHashConverter extends Converter<String, byte[]>
{
	public static StringToHashConverter New(final Supplier<PasswordHasher> passwordHasherSupplier)
	{
		return new Default(passwordHasherSupplier);
	}

	public static class Default implements StringToHashConverter
	{
		private final Supplier<PasswordHasher> passwordHasherSupplier;

		protected Default(final Supplier<PasswordHasher> passwordHasherSupplier)
		{
			super();

			this.passwordHasherSupplier = requireNonNull(passwordHasherSupplier);
		}

		@Override
		public Result<byte[]> convertToModel(final String value, final ValueContext context)
		{
			if(value != null)
			{
				return Result.ok(
					this.passwordHasherSupplier.get().hashPassword(
						value.getBytes(StandardCharsets.UTF_8)));
			}

			return Result.ok(null);
		}

		@Override
		public String convertToPresentation(final byte[] value, final ValueContext context)
		{
			if(value != null)
			{
				return new String(value, StandardCharsets.UTF_8);
			}

			return null;
		}
	}
}
