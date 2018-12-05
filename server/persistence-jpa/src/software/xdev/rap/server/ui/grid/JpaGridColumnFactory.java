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

package software.xdev.rap.server.ui.grid;


import static java.util.Objects.requireNonNull;

import java.util.Collection;

import javax.persistence.metamodel.Attribute;

import com.vaadin.flow.component.grid.Grid;
import com.vaadin.flow.component.grid.Grid.Column;

import software.xdev.rap.server.persistence.jpa.AttributeChain;
import software.xdev.rap.server.persistence.jpa.Jpa;
import software.xdev.rap.server.resources.CaptionUtils;


/**
 * @author XDEV Software
 *
 */
public interface JpaGridColumnFactory
{
	public static JpaGridColumnFactory For(final Attribute<?, ?>... attributeChain)
	{
		return new Implementation(new AttributeChain(attributeChain));
	}
	
	
	public static JpaGridColumnFactory For(
			final Collection<? extends Attribute<?, ?>> attributeChain)
	{
		return new Implementation(new AttributeChain(attributeChain));
	}
	
	
	public static JpaGridColumnFactory For(final AttributeChain attributeChain)
	{
		return new Implementation(attributeChain.clone());
	}
	
	
	public <T> Column<T> addTo(Grid<T> grid);
	
	
	
	public static class Implementation implements JpaGridColumnFactory
	{
		private final AttributeChain attributeChain;
		
		
		public Implementation(final AttributeChain attributeChain)
		{
			super();
			
			this.attributeChain = requireNonNull(attributeChain);
		}
		
		
		@Override
		public <T> Column<T> addTo(final Grid<T> grid)
		{
			final AttributeChain attributeChain = this.attributeChain;
			final Attribute<?, ?> attribute = attributeChain.last();
			
			return grid.addColumn(entity -> Jpa.resolveValue(entity,attributeChain))
					.setHeader(CaptionUtils.resolveCaption(
							attribute.getDeclaringType().getJavaType(),attribute.getName()))
					.setKey(Jpa.toPropertyPath(attributeChain)).setSortable(true);
		}
	}
}
