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

package com.rapidclipse.framework.security.util;

import java.security.NoSuchAlgorithmException;
import java.security.spec.InvalidKeySpecException;
import java.security.spec.KeySpec;
import java.util.Random;

import javax.crypto.SecretKeyFactory;
import javax.crypto.spec.PBEKeySpec;


/**
 * @author XDEV Software
 */
public interface PasswordHasher
{
	public byte[] hashPassword(final byte[] password);
	
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
			final byte[] salt = new byte[16];
			new Random().nextBytes(salt);
			
			byte[] hash = null;
			
			try
			{
				final KeySpec          spec = new PBEKeySpec(new String(password).toCharArray(), salt, 65536,
					128);
				final SecretKeyFactory f    = SecretKeyFactory.getInstance("PBKDF2WithHmacSHA1");
				hash = f.generateSecret(spec).getEncoded();
				
			}
			catch(final NoSuchAlgorithmException | InvalidKeySpecException e)
			{
				throw new RuntimeException(e);
			}
			
			return hash;
		}
	}
}
