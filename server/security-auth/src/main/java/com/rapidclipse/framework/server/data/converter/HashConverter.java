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
package com.rapidclipse.framework.server.data.converter;

import static java.util.Objects.requireNonNull;

import java.util.function.Supplier;

import com.rapidclipse.framework.security.util.PasswordHasher;
import com.vaadin.flow.data.binder.Result;
import com.vaadin.flow.data.binder.ValueContext;
import com.vaadin.flow.data.converter.Converter;


/**
 * @author XDEV Software
 */
public interface HashConverter extends Converter<String, String>
{
	public static HashConverter Md5()
	{
		return new Default(PasswordHasher::Md5, 32);
	}

	public static HashConverter Sha1()
	{
		return new Default(PasswordHasher::Sha1, 40);
	}

	public static HashConverter Sha2()
	{
		return new Default(PasswordHasher::Sha2, 64);
	}

	public static HashConverter Pbkdf2withHmacSha1()
	{
		return new Default(PasswordHasher::Pbkdf2withHmacSha1, 32);
	}

	public static class Default implements HashConverter
	{
		private final Supplier<PasswordHasher> passwordHasherSupplier;
		private final int                      hashLength;

		protected Default(
			final Supplier<PasswordHasher> passwordHasherSupplier,
			final int hashLength)
		{
			this.passwordHasherSupplier = requireNonNull(passwordHasherSupplier);
			this.hashLength             = hashLength;
		}

		@Override
		public Result<String> convertToModel(final String value, final ValueContext context)
		{
			if(value == null || value.length() == 0)
			{
				return Result.ok(null);
			}

			final byte[] bytes = value.getBytes();

			if(bytes.length == this.hashLength)
			{
				return Result.ok(value);
			}

			return Result
				.ok(Hex.encodeToString(this.passwordHasherSupplier.get().hashPassword(bytes)));
		}

		@Override
		public String convertToPresentation(final String value, final ValueContext context)
		{
			return value;
		}

		public static class Hex
		{
			/**
			 * Used to build output as Hex
			 */
			private static final char[] DIGITS = {'0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b',
				'c', 'd', 'e', 'f'};

			/**
			 * Encodes the specified byte array to a character array and then
			 * returns that character array as a String.
			 *
			 * @param bytes
			 *            the byte array to Hex-encode.
			 * @return A String representation of the resultant hex-encoded char
			 *         array.
			 */
			public static String encodeToString(final byte[] bytes)
			{
				return new String(encode(bytes));
			}

			/**
			 * Converts an array of bytes into an array of characters
			 * representing the hexadecimal values of each byte in order. The
			 * returned array will be double the length of the passed array, as
			 * it takes two characters to represent any given byte.
			 *
			 * @param data
			 *            byte[] to convert to Hex characters
			 * @return A char[] containing hexadecimal characters
			 */
			public static char[] encode(final byte[] data)
			{
				final int l = data.length;

				final char[] out = new char[l << 1];

				// two characters form the hex value.
				for(int i = 0, j = 0; i < l; i++)
				{
					out[j++] = DIGITS[(0xF0 & data[i]) >>> 4];
					out[j++] = DIGITS[0x0F & data[i]];
				}

				return out;
			}
		}
	}
}
