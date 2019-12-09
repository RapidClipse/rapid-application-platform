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
package com.rapidclipse.framework.security.util;

import java.nio.charset.StandardCharsets;
import java.security.NoSuchAlgorithmException;
import java.security.SecureRandom;
import java.security.spec.InvalidKeySpecException;
import java.security.spec.KeySpec;
import java.util.Arrays;

import javax.crypto.SecretKeyFactory;
import javax.crypto.spec.PBEKeySpec;


/**
 * @author XDEV Software
 */
public interface PasswordHasher
{
	public byte[] hashPassword(final byte[] password);

	/**
	 * @since 10.01.00
	 */
	public default boolean validatePassword(final byte[] password, final byte[] hash)
	{
		return Arrays.equals(hashPassword(password), hash);
	}

	public static PasswordHasher Md5()
	{
		return new MessageDigest("MD5");
	}

	public static PasswordHasher Sha1()
	{
		return new MessageDigest("SHA-1");
	}

	public static PasswordHasher Sha2()
	{
		return new MessageDigest("SHA-256");
	}

	public static PasswordHasher Pbkdf2withHmacSha1()
	{
		return new Pbkdf2withHmacSha1();
	}

	public static class MessageDigest implements PasswordHasher
	{
		private final String algorithm;

		public MessageDigest(final String algorithm)
		{
			super();

			this.algorithm = algorithm;
		}

		@Override
		public byte[] hashPassword(final byte[] password)
		{
			try
			{
				return java.security.MessageDigest.getInstance(this.algorithm).digest(password);
			}
			catch(final NoSuchAlgorithmException e)
			{
				throw new RuntimeException(e);
			}
		}
	}

	public static class Pbkdf2withHmacSha1 implements PasswordHasher
	{
		@Override
		public byte[] hashPassword(final byte[] password)
		{
			try
			{
				final byte[] salt = new byte[16];
				SecureRandom.getInstance("SHA1PRNG").nextBytes(salt);
				final int    iterations = 1000;
				final byte[] pwHash     = hashPassword(password, salt, iterations);
				final byte[] hash       = new byte[8 + salt.length + pwHash.length];
				putInt(hash, 0, iterations);
				putInt(hash, 4, salt.length);
				System.arraycopy(salt, 0, hash, 8, salt.length);
				System.arraycopy(pwHash, 0, hash, hash.length - pwHash.length, pwHash.length);
				return hash;
			}
			catch(final NoSuchAlgorithmException | InvalidKeySpecException e)
			{
				throw new RuntimeException(e);
			}
		}

		@Override
		public boolean validatePassword(final byte[] password, final byte[] hash)
		{
			try
			{
				final int    iterations = getInt(hash, 0);
				final int    saltLength = getInt(hash, 4);
				final byte[] salt       = new byte[saltLength];
				System.arraycopy(hash, 8, salt, 0, saltLength);
				final int    pwHashLength = hash.length - salt.length - 8;
				final byte[] pwHash       = new byte[pwHashLength];
				System.arraycopy(hash, hash.length - pwHash.length, pwHash, 0, pwHashLength);
				return Arrays.equals(hashPassword(password, salt, iterations), pwHash);
			}
			catch(final IndexOutOfBoundsException e)
			{
				return false;
			}
			catch(final NoSuchAlgorithmException | InvalidKeySpecException e)
			{
				throw new RuntimeException(e);
			}
		}

		private byte[] hashPassword(final byte[] password, final byte[] salt, final int iterations)
			throws NoSuchAlgorithmException, InvalidKeySpecException
		{
			final char[]           chars = new String(password, StandardCharsets.UTF_8).toCharArray();
			final KeySpec          spec  = new PBEKeySpec(chars, salt, iterations, 512);
			final SecretKeyFactory f     = SecretKeyFactory.getInstance("PBKDF2WithHmacSHA1");
			return f.generateSecret(spec).getEncoded();
		}

		private void putInt(final byte[] b, final int off, final int val)
		{
			b[off + 3] = (byte)(val);
			b[off + 2] = (byte)(val >>> 8);
			b[off + 1] = (byte)(val >>> 16);
			b[off]     = (byte)(val >>> 24);
		}

		private int getInt(final byte[] b, final int off)
		{
			return ((b[off + 3] & 0xFF)) +
				((b[off + 2] & 0xFF) << 8) +
				((b[off + 1] & 0xFF) << 16) +
				((b[off]) << 24);
		}
	}
}
