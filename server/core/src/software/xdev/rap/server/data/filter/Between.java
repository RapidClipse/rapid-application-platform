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

package software.xdev.rap.server.data.filter;


/**
 * @author XDEV Software
 *
 */
public interface Between extends Filter
{
	public Object identifier();
	
	
	public Comparable<?> start();
	
	
	public Comparable<?> end();



	public static class Implementation implements Between
	{
		private final Object		identifier;
		private final Comparable<?>	start;
		private final Comparable<?>	end;


		public Implementation(final Object identifier, final Comparable<?> start,
				final Comparable<?> end)
		{
			super();
			
			this.identifier = identifier;
			this.start = start;
			this.end = end;
		}
		
		
		@Override
		public Object identifier()
		{
			return this.identifier;
		}
		
		
		@Override
		public Comparable<?> start()
		{
			return this.start;
		}
		
		
		@Override
		public Comparable<?> end()
		{
			return this.end;
		}
	}
}
