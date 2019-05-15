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

package com.rapidclipse.framework.server.data.renderer;

import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Locale;
import java.util.Objects;

import com.vaadin.flow.data.renderer.BasicRenderer;
import com.vaadin.flow.function.ValueProvider;


/**
 * A template renderer for presenting date values.
 *
 * @author XDEV Software
 *
 * @param <SOURCE>
 *            the type of the input item, from which the {@link Date} is
 *            extracted
 */
public class DateRenderer<SOURCE> extends BasicRenderer<SOURCE, Date>
{
	private DateFormat format;
	private String     nullRepresentation;

	/**
	 * Creates a new DateRenderer.
	 * <p>
	 * The renderer is configured with the default date format
	 * and an empty string as its null representation.
	 *
	 * @param valueProvider
	 *            the callback to provide a {@link Date} to the renderer,
	 *            not <code>null</code>
	 *
	 * @see DateFormat#getDateInstance()
	 */
	public DateRenderer(final ValueProvider<SOURCE, Date> valueProvider)
	{
		this(valueProvider, DateFormat.getDateInstance(), "");
	}

	/**
	 * Creates a new DateRenderer.
	 * <p>
	 * The renderer is configured to render with the given string format, with
	 * an empty string as its null representation.
	 *
	 * @param valueProvider
	 *            the callback to provide a {@link Date} to the renderer,
	 *            not <code>null</code>
	 *
	 * @param formatPattern
	 *            the format pattern to format the date with, not
	 *            <code>null</code>
	 */
	public DateRenderer(
		final ValueProvider<SOURCE, Date> valueProvider,
		final String formatPattern)
	{
		this(valueProvider, formatPattern, Locale.getDefault());
	}

	/**
	 * Creates a new DateRenderer.
	 * <p>
	 * The renderer is configured to render with the given string format, as
	 * displayed in the given locale, with an empty string as its null
	 * representation.
	 *
	 * @param valueProvider
	 *            the callback to provide a {@link Date} to the renderer,
	 *            not <code>null</code>
	 * @param formatPattern
	 *            the format pattern to format the date with, not
	 *            <code>null</code>
	 * @param locale
	 *            the locale to use, not <code>null</code>
	 */
	public DateRenderer(
		final ValueProvider<SOURCE, Date> valueProvider,
		final String formatPattern,
		final Locale locale)
	{
		this(valueProvider, formatPattern, locale, "");
	}

	/**
	 * Creates a new DateRenderer.
	 * <p>
	 * The renderer is configured to render with the given string format, as
	 * displayed in the given locale.
	 *
	 * @param valueProvider
	 *            the callback to provide a {@link Date} to the renderer,
	 *            not <code>null</code>
	 * @param formatPattern
	 *            the format pattern to format the date with, not
	 *            <code>null</code>
	 * @param locale
	 *            the locale to use, not <code>null</code>
	 * @param nullRepresentation
	 *            the textual representation of the <code>null</code> value
	 */
	public DateRenderer(
		final ValueProvider<SOURCE, Date> valueProvider,
		final String formatPattern,
		final Locale locale,
		final String nullRepresentation)
	{
		super(valueProvider);

		this.format             = new SimpleDateFormat(
			Objects.requireNonNull(formatPattern, "format pattern may not be null"),
			Objects.requireNonNull(locale, "locale may not be null"));
		this.nullRepresentation = nullRepresentation;
	}

	/**
	 * Creates a new DateRenderer.
	 * <p>
	 * The renderer is configured to render with the given formatter, with an
	 * empty string as its null representation.
	 *
	 * @param valueProvider
	 *            the callback to provide a {@link Date} to the renderer,
	 *            not <code>null</code>
	 * @param format
	 *            the format to use, not <code>null</code>
	 */
	public DateRenderer(
		final ValueProvider<SOURCE, Date> valueProvider,
		final DateFormat format)
	{
		this(valueProvider, format, "");
	}

	/**
	 * Creates a new DateRenderer.
	 * <p>
	 * The renderer is configured to render with the given format.
	 *
	 * @param valueProvider
	 *            the callback to provide a {@link Date} to the renderer,
	 *            not <code>null</code>
	 * @param format
	 *            the format to use, not <code>null</code>
	 * @param nullRepresentation
	 *            the textual representation of the <code>null</code> value
	 *
	 */
	public DateRenderer(
		final ValueProvider<SOURCE, Date> valueProvider,
		final DateFormat format,
		final String nullRepresentation)
	{
		super(valueProvider);

		this.format = Objects.requireNonNull(format, "format may not be null");
	}

	@Override
	protected String getFormattedValue(final Date date)
	{
		try
		{
			return date == null ? this.nullRepresentation : this.format.format(date);
		}
		catch(final Exception e)
		{
			throw new IllegalStateException("Could not format input date '"
				+ date + "' using format '" + this.format + "'", e);
		}
	}
}
