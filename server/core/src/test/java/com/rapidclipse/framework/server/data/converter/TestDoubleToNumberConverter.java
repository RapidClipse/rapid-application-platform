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

import static org.junit.jupiter.api.Assertions.assertFalse;

import java.util.Random;

import org.junit.jupiter.api.Test;

import com.vaadin.flow.data.binder.ValueContext;


/**
 * @author XDEV Software
 *
 */
public class TestDoubleToNumberConverter
{
	private final double       value;
	private final ValueContext valueContext;

	public TestDoubleToNumberConverter()
	{
		final Random random = new Random();
		this.value        = random.nextDouble() * random.nextInt();
		this.valueContext = new ValueContext();
	}

	@Test
	void testDoubleToByte()
	{
		assertFalse(DoubleToNumberConverter.Byte().convertToModel(this.value, this.valueContext).isError());
	}

	@Test
	void testDoubleToShort()
	{
		assertFalse(DoubleToNumberConverter.Short().convertToModel(this.value, this.valueContext).isError());
	}

	@Test
	void testDoubleToInteger()
	{
		assertFalse(DoubleToNumberConverter.Integer().convertToModel(this.value, this.valueContext).isError());
	}

	@Test
	void testDoubleToLong()
	{
		assertFalse(DoubleToNumberConverter.Long().convertToModel(this.value, this.valueContext).isError());
	}

	@Test
	void testDoubleToFloat()
	{
		assertFalse(DoubleToNumberConverter.Float().convertToModel(this.value, this.valueContext).isError());
	}

	@Test
	void testDoubleToBigInteger()
	{
		assertFalse(DoubleToNumberConverter.BigInteger().convertToModel(this.value, this.valueContext).isError());
	}

	@Test
	void testDoubleToBigDecimal()
	{
		assertFalse(DoubleToNumberConverter.BigDecimal().convertToModel(this.value, this.valueContext).isError());
	}
}
