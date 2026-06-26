/*
 * RapidClipse Application Platform (RAP)
 *
 * Copyright (C) 2013-2024 by XDEV Software, All Rights Reserved.
 *
 * License: GNU Lesser General Public License (LGPL), version 3.0 or later.
 * See the LICENSE file in the root directory or https://www.gnu.org/licenses/lgpl+gpl-3.0.txt.
 */
package com.rapidclipse.framework.server.webapi.visibility;

import java.util.function.Consumer;

import com.rapidclipse.framework.server.webapi.JavascriptTemplate;
import com.vaadin.flow.component.ClientCallable;
import com.vaadin.flow.component.HasElement;
import com.vaadin.flow.component.Tag;
import com.vaadin.flow.component.UI;
import com.vaadin.flow.component.dependency.JsModule;
import com.vaadin.flow.function.SerializableConsumer;
import com.vaadin.flow.shared.Registration;


/**
 * With this class you can query the current visiblity of your application. You can also register listeners for when the
 * visibility status has changed. For example if the application is running in the web browser and the web browser is
 * minimized the visibility would be 'hidden'.
 * 
 * @author XDEV Software
 * @since 10.02.00
 */
@JsModule("./webapi/visibility.ts")
@Tag("rap-visibility")
public class Visibility extends JavascriptTemplate
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
}
