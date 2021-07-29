/*
 * Copyright (C) 2013-2021 by XDEV Software, All Rights Reserved.
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
 *     XDEV Software Corp. - initial API and implementation
 */
package com.rapidclipse.framework.server.webapi.visibility;

import java.util.function.Consumer;

import com.rapidclipse.framework.server.webapi.JavascriptTemplate;
import com.vaadin.flow.component.ClientCallable;
import com.vaadin.flow.component.HasElement;
import com.vaadin.flow.component.Tag;
import com.vaadin.flow.component.UI;
import com.vaadin.flow.component.dependency.HtmlImport;
import com.vaadin.flow.function.SerializableConsumer;
import com.vaadin.flow.shared.Registration;
import com.vaadin.flow.templatemodel.TemplateModel;


/**
 * With this class you can query the current visiblity of your application. You can also register listeners for when the
 * visibility status has changed. For example if the application is running in the web browser and the web browser is
 * minimized the visibility would be 'hidden'.
 * 
 * @author XDEV Software
 * @since 10.02.00
 */
@HtmlImport("frontend://webapi/visibility.html")
@Tag("rap-visibility")
public class Visibility extends JavascriptTemplate<Visibility.VisibilityTemplateModel>
{
	public Visibility(final HasElement parent)
	{
		parent.getElement().appendVirtualChild(this.getElement());
	}
	
	/**
	 * Register a onVisibilityChange listener. This listener is triggered when the visibility of your application has
	 * changed. You can unregister this listener by using the returned Registration or by calling the
	 * {@link #unregisterAllVisibilityChangeListeners()} method.
	 */
	public Registration addVisibilityChangeListener(final SerializableConsumer<VisibilityState> onVisibilityReceived)
	{
		final Runnable firstAddedCallback  = () -> this.getElement().callJsFunction("registerVisibilityListener");
		final Runnable lastRemovedCallback = () -> this.getElement().callJsFunction("unregisterVisibilityListener");
		return this.registerConsumer(VisibilityState.class, onVisibilityReceived, firstAddedCallback,
			lastRemovedCallback);
	}
	
	/**
	 * Unregister previously registered onVisibilityChange listeners. This will also stop the client from sending more
	 * events to the server. To register such listener you can call the
	 * {@link #addVisibilityChangeListener(SerializableConsumer)} method.
	 */
	public void unregisterAllVisibilityChangeListeners()
	{
		this.getElement().callJsFunction("unregisterVisibilityListener");
		this.clearConsumers();
	}
	
	/**
	 * Get the current visibility state of your application.
	 *
	 * @param onVisibilityStateReceived
	 *            The callback triggered when the visibility status was received from the client. It consumes said
	 *            visibility state.
	 */
	public static void getVisibilityState(final Consumer<VisibilityState> onVisibilityStateReceived)
	{
		UI.getCurrent()
			.getPage()
			.executeJs("return document.visibilityState;")
			.then(String.class, vis -> onVisibilityStateReceived.accept(VisibilityState.valueOf(vis)));
	}
	
	/**
	 * Asks the device if the application is currently hidden. This is a convinient method for the
	 * {@link #getVisibilityState(Consumer)} method.
	 *
	 * @param onStatusReceived
	 *            The callback triggered when the status is received from the client. It consumes said status.
	 */
	public static void isHidden(final SerializableConsumer<Boolean> onStatusReceived)
	{
		UI.getCurrent()
			.getPage()
			.executeJs("return document.hidden")
			.then(Boolean.class, onStatusReceived);
	}
	
	@ClientCallable
	private void onVisibilityChanged()
	{
		Visibility.getVisibilityState(state -> this.notifyConsumers(VisibilityState.class, state));
	}
	
	public static interface VisibilityTemplateModel extends TemplateModel
	{
	}
}
