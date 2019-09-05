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
