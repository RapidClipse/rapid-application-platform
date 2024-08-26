/*
 * RapidClipse Application Platform (RAP)
 *
 * Copyright (C) 2013-2024 by XDEV Software, All Rights Reserved.
 *
 * License: GNU Lesser General Public License (LGPL), version 3.0 or later.
 * See the LICENSE file in the root directory or https://www.gnu.org/licenses/lgpl+gpl-3.0.txt.
 */
package com.rapidclipse.framework.server.webapi.touch;

import com.rapidclipse.framework.server.webapi.JavascriptTemplate;
import com.rapidclipse.framework.server.webapi.JsonUtils;
import com.vaadin.flow.component.ClientCallable;
import com.vaadin.flow.component.HasElement;
import com.vaadin.flow.component.Tag;
import com.vaadin.flow.component.dependency.JsModule;
import com.vaadin.flow.function.SerializableConsumer;
import com.vaadin.flow.shared.Registration;

import elemental.json.JsonObject;


/**
 * This class allows the server to register touch event listeners. These listeners will receive the coordinates of the
 * touch events.
 * 
 * @author XDEV Software
 * @since 10.02.00
 */
@JsModule("./webapi/touch.ts")
@Tag("rap-touch")
public class Touch extends JavascriptTemplate
{
	private final HasElement target;
	
	/**
	 *
	 * @param target
	 *            the target to register the touch listener on
	 */
	public Touch(final HasElement target)
	{
		super(target);
		
		this.target = target;
	}
	
	/**
	 * Registers a touch listener.
	 *
	 * @param touchListener
	 *            The callback that is triggered when the event occurs. It will consume a TouchEvent.
	 */
	public Registration addTouchListener(final SerializableConsumer<TouchEvent> touchListener)
	{
		final Runnable firstAddedCallback  =
			() -> this.getElement().callJsFunction("registerTouchListener", this.target);
		final Runnable lastRemovedCallback =
			() -> this.getElement().callJsFunction("unregisterTouchListener", this.target);
		return this.registerConsumer(TouchEvent.class, touchListener, firstAddedCallback, lastRemovedCallback);
	}
	
	@ClientCallable
	private void onTouchStart(final JsonObject event)
	{
		this.notifyTouchListener(event, TouchEvent.Type.TOUCH_START);
	}
	
	@ClientCallable
	private void onTouchEnd(final JsonObject event)
	{
		this.notifyTouchListener(event, TouchEvent.Type.TOUCH_END);
	}
	
	@ClientCallable
	private void onTouchMove(final JsonObject event)
	{
		this.notifyTouchListener(event, TouchEvent.Type.TOUCH_MOVE);
	}
	
	@ClientCallable
	private void onTouchCancel(final JsonObject event)
	{
		this.notifyTouchListener(event, TouchEvent.Type.TOUCH_CANCEL);
	}
	
	private void notifyTouchListener(final JsonObject eventObj, final TouchEvent.Type eventType)
	{
		final TouchEvent event = JsonUtils.GSON.fromJson(eventObj.toJson(), TouchEvent.class);
		event.type = eventType;
		this.notifyConsumers(TouchEvent.class, event);
	}
}
