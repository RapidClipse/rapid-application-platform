
package com.rapidclipse.framework.security.util;

import static org.junit.jupiter.api.Assertions.assertTrue;

import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;

import org.junit.jupiter.api.Test;


/**
 * @author XDEV Software
 *
 */
public class TestPasswordHasher
{
	private final static Charset CHARSET = StandardCharsets.UTF_8;

	private final byte[][] passwords;

	public TestPasswordHasher()
	{
		this.passwords = new byte[][]{
			"".getBytes(CHARSET),
			"123".getBytes(CHARSET),
			"geheim_456".getBytes(CHARSET),
			"PN()/h9uI_'#bkfblks.df".getBytes(CHARSET),
			"j(_4p98znfiu_ \"§4pbh9z7IUB-(U/ZIsafhg".getBytes(CHARSET)
		};
	}

	@Test
	void testMd5()
	{
		testPasswords(PasswordHasher.Md5());
	}

	@Test
	void testSha1()
	{
		testPasswords(PasswordHasher.Sha1());
	}

	@Test
	void testSha2()
	{
		testPasswords(PasswordHasher.Sha2());
	}

	@Test
	void testPbkdf2withHmacSha1()
	{
		testPasswords(PasswordHasher.Pbkdf2withHmacSha1());
	}

	void testPasswords(final PasswordHasher hasher)
	{
		for(final byte[] password : this.passwords)
		{
			final byte[]  hash  = hasher.hashPassword(password);
			final boolean valid = hasher.validatePassword(password, hash);
			assertTrue(valid,
				() -> hasher.getClass().getName() + " with " + new String(password, CHARSET));
		}
	}
}
