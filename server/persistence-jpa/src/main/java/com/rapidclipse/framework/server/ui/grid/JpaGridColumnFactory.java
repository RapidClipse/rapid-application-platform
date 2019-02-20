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

package com.rapidclipse.framework.server.ui.grid;

import static java.util.Objects.requireNonNull;

import java.util.Collection;

import javax.persistence.metamodel.Attribute;

import com.rapidclipse.framework.server.persistence.jpa.AttributeChain;
import com.rapidclipse.framework.server.persistence.jpa.Jpa;
import com.rapidclipse.framework.server.resources.CaptionUtils;
import com.vaadin.flow.component.grid.Grid;
import com.vaadin.flow.component.grid.Grid.Column;


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
			final AttributeChain  attributeChain = this.attributeChain;
			final Attribute<?, ?> attribute      = attributeChain.last();
			
			return grid.addColumn(entity -> Jpa.resolveValue(entity, attributeChain))
				.setHeader(CaptionUtils.resolveCaption(
					attribute.getDeclaringType().getJavaType(), attribute.getName()))
				.setKey(Jpa.toPropertyPath(attributeChain)).setSortable(true);
		}
	}
}
