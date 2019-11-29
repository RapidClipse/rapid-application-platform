
package com.rapidclipse.framework.server.charts;

import static java.util.Objects.requireNonNull;

import java.io.Serializable;
import java.time.LocalDate;
import java.time.LocalDateTime;

import com.rapidclipse.framework.server.util.JavaScriptable;


/**
 * @author XDEV Software
 * @since 10.02.00
 *
 */
public interface DateValue extends Serializable, JavaScriptable
{
	public static DateValue Date(final LocalDate value)
	{
		requireNonNull(value);
		return () -> Static.js(value);
	}

	public static DateValue DateTime(final LocalDateTime value)
	{
		requireNonNull(value);
		return () -> Static.js(value);
	}

	public static DateValue Timestamp(final long timestamp)
	{
		return () -> Static.js(timestamp);
	}
}
