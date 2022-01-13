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
package com.rapidclipse.framework.server.webapi.touch;

import java.lang.reflect.Type;

import com.google.gson.reflect.TypeToken;
import com.rapidclipse.framework.server.webapi.JavascriptTemplate;
import com.rapidclipse.framework.server.webapi.JsonUtils;
import com.vaadin.flow.component.ClientCallable;
import com.vaadin.flow.component.HasElement;
import com.vaadin.flow.component.Tag;
import com.vaadin.flow.component.dependency.HtmlImport;
import com.vaadin.flow.function.SerializableConsumer;
import com.vaadin.flow.shared.Registration;
import com.vaadin.flow.templatemodel.TemplateModel;

import elemental.json.JsonObject;


/**
 * This class allows the server to register touch event listeners. These listeners will receive the coordinates of the
 * touch events.
 * 
 * @author XDEV Software
 * @since 10.02.00
 */
@HtmlImport("frontend://webapi/touch.html")
@Tag("rap-touch")
public class Touch extends JavascriptTemplate<Touch.TouchTemplateModel>
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
		final Type       t     = new TypeToken<TouchEvent>()
								{}.getType();
		final TouchEvent event = JsonUtils.GSON.fromJson(eventObj.toJson(), t);
		event.type = eventType;
		this.notifyConsumers(TouchEvent.class, event);
	}
	
	public static interface TouchTemplateModel extends TemplateModel
	{
	}
}
