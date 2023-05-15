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

import java.time.LocalDate;

import org.junit.jupiter.api.Test;

import com.vaadin.flow.data.binder.ValueContext;


/**
 * @author XDEV Software
 *
 */
public class TestLocalDateToTemporalConverter
{
	private final LocalDate    value        = LocalDate.now();
	private final ValueContext valueContext = new ValueContext();

	@Test
	void testLocalDateToLocalDate()
	{
		assertFalse(LocalDateToTemporalConverter.LocalDate().convertToModel(this.value, this.valueContext).isError());
	}

	@Test
	void testLocalDateToLocalTime()
	{
		assertFalse(LocalDateToTemporalConverter.LocalTime().convertToModel(this.value, this.valueContext).isError());
	}

	@Test
	void testLocalDateToLocalDateTime()
	{
		assertFalse(
			LocalDateToTemporalConverter.LocalDateTime().convertToModel(this.value, this.valueContext).isError());
	}

	@Test
	void testLocalDateToOffsetTime()
	{
		assertFalse(LocalDateToTemporalConverter.OffsetTime().convertToModel(this.value, this.valueContext).isError());
	}

	@Test
	void testLocalDateToOffsetDateTime()
	{
		assertFalse(
			LocalDateToTemporalConverter.OffsetDateTime().convertToModel(this.value, this.valueContext).isError());
	}

	@Test
	void testLocalDateToZonedDateTime()
	{
		assertFalse(
			LocalDateToTemporalConverter.ZonedDateTime().convertToModel(this.value, this.valueContext).isError());
	}

	@Test
	void testLocalDateToYear()
	{
		assertFalse(LocalDateToTemporalConverter.Year().convertToModel(this.value, this.valueContext).isError());
	}

	@Test
	void testLocalDateToYearMonth()
	{
		assertFalse(LocalDateToTemporalConverter.YearMonth().convertToModel(this.value, this.valueContext).isError());
	}
}
