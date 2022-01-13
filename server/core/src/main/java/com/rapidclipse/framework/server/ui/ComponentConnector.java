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
package com.rapidclipse.framework.server.ui;

import com.vaadin.flow.component.Component;
import com.vaadin.flow.component.grid.Grid;
import com.vaadin.flow.data.binder.Binder;
import com.vaadin.flow.data.selection.SingleSelect;
import com.vaadin.flow.shared.Registration;


/**
 * @author XDEV Software
 *
 */
public final class ComponentConnector
{
	public static <T> Registration connect(final Grid<T> master, final Binder<? super T> detail)
	{
		return connect(master.asSingleSelect(), detail);
	}

	public static <C extends Component, T> Registration connect(
		final SingleSelect<C, T> master,
		final Binder<? super T> detail)
	{
		return master.addValueChangeListener(event -> detail.setBean(event.getValue()));
	}

	public static <CD extends Component, T> Registration connect(
		final Grid<T> master,
		final SingleSelect<CD, ? super T> detail)
	{
		return connect(master.asSingleSelect(), detail);
	}

	public static <CM extends Component, T> Registration connect(
		final SingleSelect<CM, T> master,
		final Grid<? super T> detail)
	{
		return connect(master, detail.asSingleSelect());
	}

	public static <T> Registration connect(final Grid<T> master, final Grid<? super T> detail)
	{
		return connect(master.asSingleSelect(), detail.asSingleSelect());
	}

	public static <CM extends Component, CD extends Component, T> Registration connect(
		final SingleSelect<CM, T> master,
		final SingleSelect<CD, ? super T> detail)
	{
		return master.addValueChangeListener(event -> detail.setValue(event.getValue()));
	}

	private ComponentConnector()
	{
		throw new Error();
	}
}
