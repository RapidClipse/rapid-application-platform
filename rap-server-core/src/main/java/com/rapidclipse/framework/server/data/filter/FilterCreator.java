/*
 * Copyright (C) 2013-2022 by XDEV Software, All Rights Reserved.
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
package com.rapidclipse.framework.server.data.filter;

import java.util.Objects;

import org.apache.commons.lang3.StringUtils;

import com.rapidclipse.framework.server.data.filter.Comparison.StringComparison;
import com.rapidclipse.framework.server.resources.Caption;
import com.rapidclipse.framework.server.resources.StringResourceUtils;
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
		return CaptionBased(captionAnnotation != null ? captionAnnotation.value() : null);
	}
	
	public static FilterCreator CaptionBased(final String caption)
	{
		if(StringUtils.isBlank(caption))
		{
			return string -> null;
		}
		return new CaptionBased(caption);
	}

	/**
	 * @since 10.04.01
	 */
	public static FilterCreator PropertyBased(final String... properties)
	{
		return new PropertyBased(Objects.requireNonNull(properties));
	}

	public static abstract class Abstract implements FilterCreator
	{
		@Override
		public Filter apply(String string)
		{
			if(string == null
				|| (string = string.trim()).length() == 0
				|| (string.length() == 1 && string.charAt(0) == StringComparison.DEFAULT_WILDCARD))
			{
				// null, empty string or only wildcard: no filtering necessary
				return null;
			}

			if(string.charAt(string.length() - 1) != StringComparison.DEFAULT_WILDCARD)
			{
				// ensure wildcard at end
				string += StringComparison.DEFAULT_WILDCARD;
			}
			
			return this.applyInternal(string);
		}
		
		protected abstract Filter applyInternal(String string);
	}

	public static class CaptionBased extends Abstract
	{
		private final String caption;
		
		CaptionBased(final String caption)
		{
			super();
			
			this.caption = caption;
		}
		
		@Override
		protected Filter applyInternal(final String string)
		{
			final Filter[] filter = new Filter[1];

			StringResourceUtils.format(this.caption, key -> {
				final Filter parameterFilter = Filter.StringComparison(key, string, false);
				filter[0] = filter[0] == null
					? parameterFilter
					: Filter.Or(filter[0], parameterFilter);

				return key;
			});

			return filter[0];
		}
	}

	/**
	 * @since 10.04.01
	 */
	public static class PropertyBased extends Abstract
	{
		private final String[] properties;
		
		PropertyBased(final String[] properties)
		{
			super();
			this.properties = properties;
		}
		
		@Override
		protected Filter applyInternal(final String string)
		{
			Filter filter = null;
			
			for(final String property : this.properties)
			{
				final Filter propertyFilter = Filter.StringComparison(property, string, false);
				filter = filter == null
					? propertyFilter
					: Filter.Or(filter, propertyFilter);
			}
			
			return filter;
		}
	}
	
}
