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

package com.rapidclipse.framework.server.mobilekit.compass;

import java.util.function.Consumer;

import com.rapidclipse.framework.server.mobilekit.MobileComponent;
import com.rapidclipse.framework.server.mobilekit.MobileServiceError;
import com.vaadin.flow.component.ClientCallable;
import com.vaadin.flow.component.Tag;
import com.vaadin.flow.component.dependency.HtmlImport;

import elemental.json.JsonObject;


/**
 * @author XDEV Software
 *
 */
@Tag("mobilekit-compass")
@HtmlImport("compass.html")
public class CompassComponent extends MobileComponent implements CompassService
{
	public CompassComponent()
	{
		super();
	}
	
	@Override
	public void getCurrentHeading(
		final CompassOptions options,
		final Consumer<Heading> successCallback,
		final Consumer<MobileServiceError> errorCallback)
	{
		final String id = registerCall(successCallback, errorCallback);
		getElement().callFunction("getCurrentHeading", id, toJson(options));
	}
	
	@ClientCallable
	void getCurrentHeading_success(final String id, final JsonObject headingObj)
	{
		final Heading heading = toJava(headingObj, HeadingImpl.class);
		getAndRemoveCall(id).success(heading);
	}
	
	@ClientCallable
	void getCurrentHeading_error(final String id, final String errorMessage)
	{
		final MobileServiceError error = new MobileServiceError(this, errorMessage);
		getAndRemoveCall(id).error(error);
	}
	
	@Override
	public void watchHeading(
		final CompassOptions options,
		final Consumer<HeadingWatch> successCallback,
		final Consumer<MobileServiceError> errorCallback)
	{
		final String id = registerCall(successCallback, errorCallback);
		getElement().callFunction("watchHeading", id, toJson(options));
	}
	
	@ClientCallable
	void watchHeading_success(
		final String id,
		final JsonObject headingObj,
		final String watchId)
	{
		final Heading      heading = toJava(headingObj, HeadingImpl.class);
		final HeadingWatch watch   = new HeadingWatchImpl(heading, id, watchId);
		getCall(id).success(watch);
	}
	
	@ClientCallable
	void watchHeading_error(final String id, final String errorMessage)
	{
		final MobileServiceError error = new MobileServiceError(this, errorMessage);
		getAndRemoveCall(id).error(error);
	}
	
	private void clearWatch(final String id, final String watchId)
	{
		removeCall(id);
		getElement().callFunction("clearWatch", watchId);
	}
	
	private static class HeadingImpl implements Heading
	{
		private final double magneticHeading;
		private final double trueHeading;
		private final double headingAccuracy;
		private final long   timestamp;
		
		@SuppressWarnings("unused") // Used by Gson via reflection
		HeadingImpl(
			final double magneticHeading,
			final double trueHeading,
			final double headingAccuracy,
			final long timestamp)
		{
			this.magneticHeading = magneticHeading;
			this.trueHeading     = trueHeading;
			this.headingAccuracy = headingAccuracy;
			this.timestamp       = timestamp;
		}
		
		@Override
		public double getMagneticHeading()
		{
			return this.magneticHeading;
		}
		
		@Override
		public double getTrueHeading()
		{
			return this.trueHeading;
		}
		
		@Override
		public double getHeadingAccuracy()
		{
			return this.headingAccuracy;
		}
		
		@Override
		public long getTimestamp()
		{
			return this.timestamp;
		}
		
		@Override
		public String toString()
		{
			return "Heading [magneticHeading=" + this.magneticHeading + ", trueHeading="
				+ this.trueHeading + ", headingAccuracy=" + this.headingAccuracy + ", timestamp="
				+ this.timestamp + "]";
		}
	}
	
	private class HeadingWatchImpl implements HeadingWatch
	{
		private final Heading heading;
		private final String  id;
		private final String  watchId;
		
		HeadingWatchImpl(
			final Heading heading,
			final String id,
			final String watchId)
		{
			super();
			this.heading = heading;
			this.id      = id;
			this.watchId = watchId;
		}
		
		@Override
		public Heading getHeading()
		{
			return this.heading;
		}
		
		@Override
		public void remove()
		{
			CompassComponent.this.clearWatch(this.id, this.watchId);
		}
		
		@Override
		public String toString()
		{
			return "HeadingWatch [heading=" + this.heading + "]";
		}
	}
}
