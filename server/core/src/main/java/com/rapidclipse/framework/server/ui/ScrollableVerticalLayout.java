/*
 * Copyright (C) 2013-2019 by XDEV Software, All Rights Reserved.
 *
 * This file is part of the RapidClipse Application Platform (RAP).
 *
 * RAP is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * RAP is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with RAP. If not, see <http://www.gnu.org/licenses/>.
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *
 * Contributors:
 *     XDEV Software Corp. - initial API and implementation
 */
package com.rapidclipse.framework.server.ui;

import com.vaadin.flow.component.Component;
import com.vaadin.flow.component.Composite;
import com.vaadin.flow.component.HasOrderedComponents;
import com.vaadin.flow.component.HasSize;
import com.vaadin.flow.component.HasStyle;
import com.vaadin.flow.component.orderedlayout.VerticalLayout;


/**
 * @author XDEV Software
 *
 */
public class ScrollableVerticalLayout extends Composite<VerticalLayout>
	implements HasOrderedComponents<ScrollableVerticalLayout>, HasStyle, HasSize
{
	public ScrollableVerticalLayout()
	{
		super();
		
		getStyle().set("overflow", "auto");
		
		final VerticalLayout content = getContent();
		content.setWidth("100%");
		content.setHeight(null);
		content.setPadding(false);
	}
	
	public ScrollableVerticalLayout(final Component... components)
	{
		this();
		
		add(components);
	}
	
	@Override
	public void add(final Component... components)
	{
		getContent().add(components);
	}
	
	@Override
	public void remove(final Component... components)
	{
		getContent().remove(components);
	}
	
	@Override
	public void removeAll()
	{
		getContent().removeAll();
	}
	
	@Override
	public void addComponentAtIndex(final int index, final Component component)
	{
		getContent().addComponentAtIndex(index, component);
	}
	
	@Override
	public void addComponentAsFirst(final Component component)
	{
		getContent().addComponentAsFirst(component);
	}
	
	@Override
	public void replace(final Component oldComponent, final Component newComponent)
	{
		getContent().replace(oldComponent, newComponent);
	}
	
	@Override
	public int indexOf(final Component component)
	{
		return getContent().indexOf(component);
	}
	
	@Override
	public int getComponentCount()
	{
		return getContent().getComponentCount();
	}
	
	@Override
	public Component getComponentAt(final int index)
	{
		return getContent().getComponentAt(index);
	}
}
