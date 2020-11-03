/*
 * Copyright (C) 2013-2020 by XDEV Software, All Rights Reserved.
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
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
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
