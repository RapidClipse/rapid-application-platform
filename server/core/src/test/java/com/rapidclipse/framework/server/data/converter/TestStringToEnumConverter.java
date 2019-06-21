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

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;

import org.junit.jupiter.api.Test;

import com.vaadin.flow.data.binder.ValueContext;


/**
 * @author XDEV Software
 *
 */
public class TestStringToEnumConverter
{
	public static enum TestEnum
	{
		EMPTY_VALUE,
		TEST_VALUE
	}

	@Test
	void testStringToEnum()
	{
		final StringToEnumConverter<TestEnum> converter    =
			new StringToEnumConverter<>(TestEnum.class, TestEnum.EMPTY_VALUE, "Error");
		final ValueContext                    valueContext = new ValueContext();
		
		assertFalse(converter.convertToModel(TestEnum.TEST_VALUE.name(), valueContext).isError());
		assertEquals(TestEnum.EMPTY_VALUE,
			converter.convertToModel("", valueContext).getOrThrow(RuntimeException::new));
	}
}
