/*
 * Copyright (C) 2013-2018 by XDEV Software, All Rights Reserved.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 *
 * For further information see
 * <http://www.rapidclipse.com/en/legal/license/license.html>.
 */

package software.xdev.rap.server.ui.navigation;


import java.util.HashMap;
import java.util.Map;


/**
 * @author XDEV Software
 *
 */
public final class UrlParameterRegistry
{
	private final Map<UrlParameterKey, Object> map = new HashMap<>();
	
	
	public UrlParameterRegistry()
	{
		super();
	}
	
	
	public UrlParameterRegistry put(final UrlParameterKey key, final Object value)
	{
		this.map.put(key,value);
		
		return this;
	}
	
	
	public Object get(final UrlParameterKey key)
	{
		return this.map.get(key);
	}
	
	
	public void removeAll(final Class<?> viewType)
	{
		this.map.keySet().removeIf(key -> key.viewType().equals(viewType));
	}
}
