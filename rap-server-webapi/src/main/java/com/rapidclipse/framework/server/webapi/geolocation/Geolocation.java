/*
 * Copyright (C) 2013-2023 by XDEV Software, All Rights Reserved.
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
package com.rapidclipse.framework.server.webapi.geolocation;

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
 * This class lets you query the geographic position of the current device and
 * also register onChange update listeners.
 *
 * @author XDEV Software
 * @since 10.02.00
 */
@JsModule("./webapi/geolocation.ts")
@Tag("rap-geolocation")
public class Geolocation extends JavascriptTemplate
{
	public Geolocation(final HasElement parent)
	{
		super(parent);
	}

	/**
	 * Add a position listener. This consumer is triggered when
	 * {@link #watchPosition(PositionOptions)} returns a position successfully.
	 */
	public Registration addPositionListener(final SerializableConsumer<Position> listener)
	{
		return this.registerConsumer(Position.class, listener);
	}

	/**
	 * Add a position error consumer. This consumer is triggered when
	 * {@link #watchPosition(PositionOptions)} returns an error.
	 */
	public Registration addPositionErrorConsumer(final SerializableConsumer<PositionError> consumer)
	{
		return this.registerConsumer(PositionError.class, consumer);
	}

	/**
	 * Ask the device for its current geographic location. When the information is
	 * received, the consumers added via
	 * {@link #addPositionListener(SerializableConsumer)} are called. If errors are
	 * received the consumers added via
	 * {@link #addPositionErrorConsumer(SerializableConsumer)} are called.
	 *
	 * @param options Various options used for the query. If a simple read suffices
	 *                the {@link PositionOptions#Default()} can be used.
	 */
	public void getCurrentPosition(final PositionOptions options)
	{
		this.getElement()
			.callJsFunction("getCurrentPosition", JsonUtils.encodeObject(options));
	}

	/**
	 * Queries the geographic location of the device. When the location is received
	 * the consumers added via {@link #addPositionListener(SerializableConsumer)}
	 * are called. If an error is received the consumers added via
	 * {@link #addPositionErrorConsumer(SerializableConsumer)} are called.
	 *
	 * @param options Various options that are used for the location query.
	 */
	public Registration watchPosition(final PositionOptions options)
	{
		this.getElement().callJsFunction("watchPosition", JsonUtils.encodeObject(options));
		return this::clearWatch;
	}

	/**
	 * Clears the listeners registered with the addPosition... methods and stops the
	 * client from sending more events to the server.
	 */
	public void clearWatch()
	{
		this.getElement().callJsFunction("clearWatch");
		this.clearConsumers();
	}

	@ClientCallable
	private void onSuccess(final JsonObject positionObj)
	{
		final Position position = JsonUtils.GSON.fromJson(positionObj.toJson(), Position.class);
		this.notifyConsumers(Position.class, position);
	}

	@ClientCallable
	private void onError(final JsonObject errorObj)
	{
		final PositionError error = JsonUtils.GSON.fromJson(errorObj.toJson(), PositionError.class);
		this.notifyConsumers(PositionError.class, error);
	}
}
