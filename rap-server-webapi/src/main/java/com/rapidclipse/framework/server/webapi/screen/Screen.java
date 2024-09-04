/*
 * RapidClipse Application Platform (RAP)
 *
 * Copyright (C) 2013-2024 by XDEV Software, All Rights Reserved.
 *
 * License: GNU Lesser General Public License (LGPL), version 3.0 or later.
 * See the LICENSE file in the root directory or https://www.gnu.org/licenses/lgpl+gpl-3.0.txt.
 */
package com.rapidclipse.framework.server.webapi.screen;

import java.util.function.Consumer;

import com.rapidclipse.framework.server.webapi.JavascriptTemplate;
import com.rapidclipse.framework.server.webapi.JsonUtils;
import com.vaadin.flow.component.ClientCallable;
import com.vaadin.flow.component.HasElement;
import com.vaadin.flow.component.Tag;
import com.vaadin.flow.component.UI;
import com.vaadin.flow.component.dependency.JsModule;
import com.vaadin.flow.dom.Element;
import com.vaadin.flow.function.SerializableConsumer;
import com.vaadin.flow.shared.Registration;

import elemental.json.JsonObject;

/**
 * This class can request the browser to fullscreen specific elements and lock
 * the screen orientation.
 * 
 * @author XDEV Software
 * @since 10.02.00
 */
@JsModule("./webapi/screen.ts")
@Tag("rap-screen")
public class Screen extends JavascriptTemplate
{
	private final HasElement target;

	public Screen(final HasElement target)
	{
		super(target);

		this.target = target;
	}

	/**
	 * Register a callback that is triggered everytime the devices screen
	 * orientation has changed. This listener can be unregistered via the returned
	 * registration.
	 *
	 * @param listener The listener triggered when the screen orientation is
	 *                 received from the client. It will consume the current screen
	 *                 orientation.
	 */
	public Registration addScreenOrientationListener(final SerializableConsumer<ScreenOrientation> listener)
	{
		final Runnable firstAddedCallback = () -> this.getElement()
			.callJsFunction("registerOnScreenOrientationChangeListener");
		final Runnable lastRemovedCallback = () -> this.getElement()
			.callJsFunction("unregisterOnScreenOrientationChangeListener");
		return this.registerConsumer(ScreenOrientation.class, listener, firstAddedCallback, lastRemovedCallback);
	}

	/**
	 * This listener is triggered when a call to
	 * {@link #requestFullscreen(Element, FullscreenOptions)} returned an error. The
	 * consumer can be removed with the returned Registration.
	 */
	public Registration addFullscreenErrorListener(final SerializableConsumer<FullscreenError> listener)
	{
		return this.registerConsumer(FullscreenError.class, listener);
	}

	/**
	 * This listener is triggered when a call to {@link #exitFullscreen()} returned
	 * an error. The consumer can be removed with the returned Registration.
	 */
	public Registration addFullscreenExitErrorListener(final SerializableConsumer<FullscreenExitError> listener)
	{
		return this.registerConsumer(FullscreenExitError.class, listener);
	}

	/**
	 * Asks the device if fullscreening is available.
	 *
	 * @param callback The callback triggered when the result is returned to the
	 *                 server. It consumes the Boolean indicating if the device
	 *                 supports fullscreening or not.
	 */
	public static void fullscreenEnabled(final SerializableConsumer<Boolean> callback)
	{
		UI.getCurrent().getPage().executeJs("return document.fullscreenEnabled;").then(Boolean.class, callback);
	}

	/**
	 * Asks the user agent to place the target element (and, by extension, its
	 * descendants) into full-screen mode. If an error occurs the onFullscreenError
	 * callbacks registered via the
	 * {@link #addFullscreenErrorListener(SerializableConsumer)} method are
	 * triggered. Fullscreen can be exited with the
	 * {@link #exitFullscreen(Consumer)} method.
	 *
	 * @param options Currently only contains the option if the navigation ui of the
	 *                browser should be shown or not.
	 */
	public void requestFullscreen(final FullscreenOptions options)
	{
		this.getElement().callJsFunction("requestFullscreen", this.target, JsonUtils.encodeObject(options));
	}

	/**
	 * This method can be called to exit a previously fullscreened element. If this
	 * operation fails the onFullscreenExitError callbacks registered via the
	 * {@link #addFullscreenExitErrorListener(SerializableConsumer)} method are
	 * triggered. To fullscreen an element the
	 * {@link #requestFullscreen(FullscreenOptions)} method can be called.
	 */
	public void exitFullscreen()
	{
		this.getElement().callJsFunction("exitFullscreen");
	}

	/**
	 * Ask the device in which screen orientation it currently is. When this
	 * information is received the <b>onScreenOrientationReceived</b> callback is
	 * triggered.
	 *
	 * @param callback The callback triggered when the screen orientation is
	 *                 received. It will consume the current screen orientation.
	 */
	public static void getScreenOrientation(final Consumer<ScreenOrientation> callback)
	{
		UI.getCurrent()
			.getPage()
			.executeJs("const so = screen.orientation; return {type: so.type, angle: so.angle };")
			.then(obj -> callback.accept(JsonUtils.GSON.fromJson(obj.toJson(), ScreenOrientation.class)));
	}

	/**
	 * Unregister all previously registered listeners. This will also stop the
	 * client from sending more events to the server.
	 */
	public void unregisterAllScreenOrientationListeners()
	{
		this.getElement().callJsFunction("unregisterOnScreenOrientationChangeListener");
		this.clearConsumers();
	}

	/**
	 * Lock the current screen orientation. You can unlock the screen by exiting
	 * fullscreen with the {@link #exitFullscreen()} method or by unlocking the
	 * current screen orientation with the {@link #unlockScreenOrientation()}
	 * method<br>
	 * <b>Note:</b> Fullscreen has to be engaged for this method to work. You can
	 * fullscreen an element with the
	 * {@link #requestFullscreen(Element, FullscreenOptions)} method.
	 *
	 * @param orientation The orientation the screen should be locked in.
	 */
	public static void lockScreenOrientation(final LockOrientation orientation)
	{
		UI.getCurrent().getPage().executeJs("screen.orientation.lock($0)", orientation.toString());
	}

	/**
	 * Unlock the screen orientation. You can lock in a screen orientation with the
	 * {@link #lockScreenOrientation(LockOrientation)} method.
	 */
	public static void unlockScreenOrientation()
	{
		UI.getCurrent().getPage().executeJs("screen.orientation.unlock()");
	}

	@ClientCallable
	private void onFullscreenFailed(final JsonObject err)
	{
		this.notifyConsumers(FullscreenError.class, JsonUtils.GSON.fromJson(err.toJson(), FullscreenError.class));
	}

	@ClientCallable
	private void onFullscreenExitFailed(final JsonObject err)
	{
		this.notifyConsumers(
			FullscreenExitError.class,
			JsonUtils.GSON.fromJson(err.toJson(), FullscreenExitError.class)
		);
	}

	@ClientCallable
	private void onScreenOrientationChange(final JsonObject orientation)
	{
		this.notifyConsumers(
			ScreenOrientation.class,
			JsonUtils.GSON.fromJson(orientation.toJson(), ScreenOrientation.class)
		);
	}

	public enum LockOrientation
	{
		any, natural, landscape, portrait, portrait_primary, portrait_secondary, landscape_primary, landscape_secondary;

		/*
		 * These methods are for easy use to convert to and parse from JavaScript's
		 * ScreenOrientation.type This is done as Java's Enums can not contain -
		 * (dashes)
		 */

		@Override
		public String toString()
		{
			return this.name().replace('_', '-');
		}

		public static LockOrientation fromString(final String str)
		{
			return LockOrientation.valueOf(str.replace('-', '_'));
		}
	}
}
