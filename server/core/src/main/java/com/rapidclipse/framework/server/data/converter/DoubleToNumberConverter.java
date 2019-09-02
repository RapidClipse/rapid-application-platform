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

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.function.Function;

import com.vaadin.flow.data.binder.Result;
import com.vaadin.flow.data.binder.ValueContext;
import com.vaadin.flow.data.converter.Converter;


/**
 * @author XDEV Software
 *
 */
public interface DoubleToNumberConverter<MODEL extends Number> extends Converter<Double, MODEL>
{
	@SuppressWarnings("unchecked")
	public static <MODEL extends Number> DoubleToNumberConverter<MODEL> New(final Class<MODEL> clazz)
	{
		if(Byte.class.equals(clazz) || byte.class.equals(clazz))
		{
			return (DoubleToNumberConverter<MODEL>)Byte();
		}
		if(Short.class.equals(clazz) || short.class.equals(clazz))
		{
			return (DoubleToNumberConverter<MODEL>)Short();
		}
		if(Integer.class.equals(clazz) || int.class.equals(clazz))
		{
			return (DoubleToNumberConverter<MODEL>)Integer();
		}
		if(Long.class.equals(clazz) || long.class.equals(clazz))
		{
			return (DoubleToNumberConverter<MODEL>)Long();
		}
		if(Float.class.equals(clazz) || float.class.equals(clazz))
		{
			return (DoubleToNumberConverter<MODEL>)Float();
		}
		if(BigInteger.class.equals(clazz))
		{
			return (DoubleToNumberConverter<MODEL>)BigInteger();
		}
		if(BigDecimal.class.equals(clazz))
		{
			return (DoubleToNumberConverter<MODEL>)BigDecimal();
		}

		throw new IllegalArgumentException("Unsupported number type: " + clazz);
	}

	public static DoubleToNumberConverter<Byte> Byte()
	{
		return new Default<>(Double::byteValue);
	}

	public static DoubleToNumberConverter<Short> Short()
	{
		return new Default<>(Double::shortValue);
	}

	public static DoubleToNumberConverter<Integer> Integer()
	{
		return new Default<>(Double::intValue);
	}

	public static DoubleToNumberConverter<Long> Long()
	{
		return new Default<>(Double::longValue);
	}

	public static DoubleToNumberConverter<Float> Float()
	{
		return new Default<>(Double::floatValue);
	}

	public static DoubleToNumberConverter<BigInteger> BigInteger()
	{
		return new Default<>(d -> BigInteger.valueOf(d.longValue()));
	}

	public static DoubleToNumberConverter<BigDecimal> BigDecimal()
	{
		return new Default<>(BigDecimal::valueOf);
	}

	public static class Default<MODEL extends Number>
		implements DoubleToNumberConverter<MODEL>
	{
		private final Function<Double, MODEL> doubleConverter;

		protected Default(final Function<Double, MODEL> doubleConverter)
		{
			super();

			this.doubleConverter = doubleConverter;
		}

		@Override
		public Result<MODEL> convertToModel(final Double value, final ValueContext context)
		{
			if(value == null)
			{
				return Result.ok(null);
			}

			return Result.ok(this.doubleConverter.apply(value));
		}

		@Override
		public Double convertToPresentation(final MODEL value, final ValueContext context)
		{
			if(value == null)
			{
				return null;
			}

			return value.doubleValue();
		}
	}
}
