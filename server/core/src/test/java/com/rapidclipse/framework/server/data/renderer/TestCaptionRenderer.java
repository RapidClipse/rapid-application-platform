
package com.rapidclipse.framework.server.data.renderer;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;

import org.junit.jupiter.api.Test;


/**
 * @author XDEV Software
 *
 */
public class TestCaptionRenderer
{
	@Test
	void testNullSafety()
	{
		assertDoesNotThrow(() -> new CaptionRenderer<>(null).createComponent(null));
		assertDoesNotThrow(() -> new CaptionRenderer<>(item -> null).createComponent(null));
	}
}
