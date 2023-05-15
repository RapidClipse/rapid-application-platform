/*
 * Copyright (C) 2013-2023 by XDEV Software, All Rights Reserved.
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
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *
 * Contributors:
 *     XDEV Software - initial API and implementation
 */
package com.rapidclipse.framework.server.data.renderer;

import java.time.Instant;
import java.time.format.DateTimeFormatter;
import java.time.format.FormatStyle;
import java.util.Locale;
import java.util.Objects;

import com.vaadin.flow.data.renderer.BasicRenderer;
import com.vaadin.flow.function.ValueProvider;


/**
 * A template renderer for presenting instant values.
 *
 * @author XDEV Software
 *
 * @param <SOURCE>
 *            the type of the input item, from which the {@link Instant} is
 *            extracted
 *
 * @since 10.01.00
 */
public class InstantRenderer<SOURCE> extends BasicRenderer<SOURCE, Instant>
{
	private DateTimeFormatter formatter;
	private String            nullRepresentation;

	/**
	 * Creates a new InstantRenderer.
	 * <p>
	 * The renderer is configured with the format style {@code FormatStyle.LONG}
	 * and an empty string as its null representation.
	 *
	 * @param valueProvider
	 *            the callback to provide a {@link Instant} to the renderer,
	 *            not <code>null</code>
	 *
	 * @see <a href=
	 *      "https://docs.oracle.com/javase/8/docs/api/java/time/format/FormatStyle.html#LONG">
	 *      FormatStyle.LONG</a>
	 */
	public InstantRenderer(final ValueProvider<SOURCE, Instant> valueProvider)
	{
		this(valueProvider, DateTimeFormatter.ofLocalizedTime(FormatStyle.LONG), "");
	}

	/**
	 * Creates a new InstantRenderer.
	 * <p>
	 * The renderer is configured to render with the given string format, with
	 * an empty string as its null representation.
	 *
	 * @param valueProvider
	 *            the callback to provide a {@link Instant} to the renderer,
	 *            not <code>null</code>
	 *
	 * @param formatPattern
	 *            the format pattern to format the time with, not
	 *            <code>null</code>
	 *
	 * @see <a href=
	 *      "https://docs.oracle.com/javase/8/docs/api/java/time/format/DateTimeFormatter.html#patterns">
	 *      Format Pattern Syntax</a>
	 */
	public InstantRenderer(
		final ValueProvider<SOURCE, Instant> valueProvider,
		final String formatPattern)
	{
		this(valueProvider, formatPattern, Locale.getDefault());
	}

	/**
	 * Creates a new InstantRenderer.
	 * <p>
	 * The renderer is configured to render with the given string format, as
	 * displayed in the given locale, with an empty string as its null
	 * representation.
	 *
	 * @param valueProvider
	 *            the callback to provide a {@link Instant} to the renderer,
	 *            not <code>null</code>
	 * @param formatPattern
	 *            the format pattern to format the time with, not
	 *            <code>null</code>
	 * @param locale
	 *            the locale to use, not <code>null</code>
	 *
	 * @see <a href=
	 *      "https://docs.oracle.com/javase/8/docs/api/java/time/format/DateTimeFormatter.html#patterns">
	 *      Format Pattern Syntax</a>
	 */
	public InstantRenderer(
		final ValueProvider<SOURCE, Instant> valueProvider,
		final String formatPattern,
		final Locale locale)
	{
		this(valueProvider, formatPattern, locale, "");
	}

	/**
	 * Creates a new InstantRenderer.
	 * <p>
	 * The renderer is configured to render with the given string format, as
	 * displayed in the given locale.
	 *
	 * @param valueProvider
	 *            the callback to provide a {@link Instant} to the renderer,
	 *            not <code>null</code>
	 * @param formatPattern
	 *            the format pattern to format the time with, not
	 *            <code>null</code>
	 * @param locale
	 *            the locale to use, not <code>null</code>
	 * @param nullRepresentation
	 *            the textual representation of the <code>null</code> value
	 *
	 * @see <a href=
	 *      "https://docs.oracle.com/javase/8/docs/api/java/time/format/DateTimeFormatter.html#patterns">
	 *      Format Pattern Syntax</a>
	 */
	public InstantRenderer(
		final ValueProvider<SOURCE, Instant> valueProvider,
		final String formatPattern,
		final Locale locale,
		final String nullRepresentation)
	{
		super(valueProvider);

		this.formatter          = DateTimeFormatter.ofPattern(
			Objects.requireNonNull(formatPattern, "format pattern may not be null"),
			Objects.requireNonNull(locale, "locale may not be null"));
		this.nullRepresentation = nullRepresentation;
	}

	/**
	 * Creates a new InstantRenderer.
	 * <p>
	 * The renderer is configured to render with the given formatter, with an
	 * empty string as its null representation.
	 *
	 * @param valueProvider
	 *            the callback to provide a {@link Instant} to the renderer,
	 *            not <code>null</code>
	 * @param formatter
	 *            the formatter to use, not <code>null</code>
	 */
	public InstantRenderer(
		final ValueProvider<SOURCE, Instant> valueProvider,
		final DateTimeFormatter formatter)
	{
		this(valueProvider, formatter, "");
	}

	/**
	 * Creates a new InstantRenderer.
	 * <p>
	 * The renderer is configured to render with the given formatter.
	 *
	 * @param valueProvider
	 *            the callback to provide a {@link Instant} to the renderer,
	 *            not <code>null</code>
	 * @param formatter
	 *            the formatter to use, not <code>null</code>
	 * @param nullRepresentation
	 *            the textual representation of the <code>null</code> value
	 *
	 */
	public InstantRenderer(
		final ValueProvider<SOURCE, Instant> valueProvider,
		final DateTimeFormatter formatter,
		final String nullRepresentation)
	{
		super(valueProvider);
		
		this.formatter = Objects.requireNonNull(formatter, "formatter may not be null");
	}

	@Override
	protected String getFormattedValue(final Instant instant)
	{
		try
		{
			return instant == null ? this.nullRepresentation : this.formatter.format(instant);
		}
		catch(final Exception e)
		{
			throw new IllegalStateException("Could not format input instant '"
				+ instant + "' using formatter '" + this.formatter + "'", e);
		}
	}
}
