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

package com.rapidclipse.framework.server.data.filter;

import java.util.Objects;

import org.apache.commons.lang3.StringUtils;

import com.rapidclipse.framework.server.data.filter.Comparison.StringComparison;
import com.rapidclipse.framework.server.resources.Caption;
import com.vaadin.flow.function.SerializableFunction;


/**
 * @author XDEV Software
 *
 */
@FunctionalInterface
public interface FilterCreator extends SerializableFunction<String, Filter>
{
	@Override
	public Filter apply(String string);

	public static FilterCreator CaptionBased(final Class<?> type)
	{
		final Caption captionAnnotation = type.getAnnotation(Caption.class);
		return new CaptionBased(captionAnnotation != null ? captionAnnotation.value() : null);
	}

	public static FilterCreator CaptionBased(final String caption)
	{
		if(StringUtils.isBlank(caption))
		{
			return string -> null;
		}
		return new CaptionBased(caption);
	}

	public static class CaptionBased implements FilterCreator
	{
		private final String caption;

		protected CaptionBased(final String caption)
		{
			super();

			this.caption = Objects.requireNonNull(caption);
		}

		@Override
		public Filter apply(String string)
		{
			if(string == null || (string = string.trim()).length() == 0
				|| (string.length() == 1 && string.charAt(0) == StringComparison.DEFAULT_WILDCARD))
			{
				// null, empty string or only wildcard: no filtering necessary
				return null;
			}

			if(string.charAt(string.length() - 1) != StringComparison.DEFAULT_WILDCARD)
			{
				string += StringComparison.DEFAULT_WILDCARD;
			}

			Filter filter      = null;
			int    start;
			int    searchStart = 0;
			while((start = this.caption.indexOf("{%", searchStart)) >= 0)
			{
				final int end = this.caption.indexOf("}", start + 2);
				if(end > start)
				{
					final String identifier = this.caption.substring(start + 2, end);

					final Filter parameterFilter = Filter.StringComparison(identifier, string, false);
					filter = filter == null ? parameterFilter : Filter.Or(filter, parameterFilter);

					searchStart = end + 1;
				}
			}

			return filter;
		}
	}
}
