
package com.rapidclipse.framework.server.resources;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;


/**
 * @author XDEV Software
 *
 */
public class TextCaptionResolver
{
	@Test
	void testNullSafety()
	{
		assertDoesNotThrow(() -> CaptionResolver.New().resolveCaption(null, null));
	}

	@Test
	void testNullRepresentation()
	{
		assertEquals("empty", CaptionResolver.New("empty").resolveCaption(null, null));
	}
}
