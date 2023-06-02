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
package com.rapidclipse.framework.server.webapi.video;

import java.util.Base64;
import java.util.Base64.Decoder;
import java.util.List;

import com.google.gson.reflect.TypeToken;
import com.rapidclipse.framework.server.webapi.JavascriptTemplate;
import com.rapidclipse.framework.server.webapi.JsonUtils;
import com.vaadin.flow.component.ClientCallable;
import com.vaadin.flow.component.HasSize;
import com.vaadin.flow.component.PropertyDescriptor;
import com.vaadin.flow.component.PropertyDescriptors;
import com.vaadin.flow.component.Tag;
import com.vaadin.flow.component.dependency.JsModule;
import com.vaadin.flow.dom.Element;
import com.vaadin.flow.function.SerializableConsumer;
import com.vaadin.flow.shared.Registration;

import elemental.json.JsonString;
import elemental.json.JsonValue;

/**
 * The html video element that can contain multimedia such as videos. To add
 * such sources the {@link #addSource(Source)} method can be used. To add the
 * users device camera the
 * {@link #addDeviceCameraVideoSource(boolean, boolean, boolean)} method can be
 * used. You can also take pictures with the {@link #takePicture()} method.
 *
 * @author XDEV Software
 * @since 10.02.00
 */
@JsModule("./webapi/video.ts")
@Tag("rap-video")
public class Video extends JavascriptTemplate implements HasSize
{
	private final PropertyDescriptor<Boolean, Boolean> controlsProperty;
	private final PropertyDescriptor<List<Source>, List<Source>> sourcesProperty;

	public Video()
	{
		this.controlsProperty = PropertyDescriptors.propertyWithDefault("controls", true);
		this.sourcesProperty = new PropertyDescriptor<List<Source>, List<Source>>()
		{
			@Override
			public void set(Element element, List<Source> value)
			{
				element.setPropertyList(getPropertyName(), value);
			}

			@Override
			public String getPropertyName()
			{
				return "sources";
			}

			@Override
			public List<Source> get(Element element)
			{
				final var token = TypeToken.getParameterized(List.class, Source.class).getType();
				final var sourcesJson = ((JsonValue)getElement().getPropertyRaw(this.getPropertyName())).toJson();
				return JsonUtils.GSON.fromJson(sourcesJson, token);
			}
		};
	}

	/**
	 * Check if the controls attribute of the video element has been set.
	 * 
	 * @return <code>true</code> if the control attribute is set.
	 */
	public boolean getShowControls()
	{
		return this.controlsProperty.get(this);
	}

	/**
	 * Set the controls attribute of the video element.
	 * 
	 * @param showControls if <code>true</code> the controls attribute will be set,
	 *                     if <code>false</code> it will be unset.
	 * @return The video element.
	 */
	public Video setShowControls(final boolean showControls)
	{
		this.controlsProperty.set(this, showControls);
		return this;
	}

	/**
	 * Add a consumer for whenever video data is received from the client side. This
	 * can be stopped by calling {@link #stopRecording()}.
	 * 
	 * @param consumer Consumer called whenever more video data has been received.
	 * @return A registration that can cancel this subscription.
	 */
	public Registration addVideoDataConsumer(final SerializableConsumer<byte[]> consumer)
	{
		return this.registerConsumer(byte[].class, consumer);
	}

	/**
	 * Add a listener that is triggered whenever a the video source is recorded.
	 * This will be triggered when the first video data chunk is received from the
	 * client. This will provide the mime type of the video.
	 * 
	 * @param mimeTypeConsumer A consumer for the mime type of the recorded video.
	 * @return A registration that can cancel this subscription.
	 */
	public Registration addRecordStartListener(final SerializableConsumer<String> mimeTypeConsumer)
	{
		return this.registerConsumer(String.class, mimeTypeConsumer);
	}

	/**
	 * Add a consumer for the picture received event triggered by
	 * {@link #takePicture()}. It will consume the image received. The result is
	 * wrapped in an ImageWrapper that contains convenience methods such as
	 * {@link ImageWrapper#toStreamResource(String)}
	 */
	public Registration addPictureConsumer(final SerializableConsumer<ImageWrapper> consumer)
	{
		return this.registerConsumer(ImageWrapper.class, consumer);
	}

	/**
	 * Set the list of source elements for the video.
	 * 
	 * @return The video element.
	 */
	public Video setSources(final List<Source> sources)
	{
		this.sourcesProperty.set(this, sources);
		return this;
	}

	/**
	 * Get the list of source elements for the video.
	 * 
	 * @return The list of sources.
	 */
	public List<Source> getSources()
	{
		return this.sourcesProperty.get(this);
	}

	/**
	 * Add the users device camera as a source.
	 */
	public void addDeviceCameraVideoSource(
		final boolean withVideo,
		final boolean useFrontCamera,
		final boolean withAudio
	)
	{
		this.getElement().callJsFunction("addDeviceCameraVideoSource", withVideo, withAudio, useFrontCamera);
	}

	/**
	 * Start recording a video of the current source.
	 */
	public void startRecording()
	{
		this.getElement().callJsFunction("startRecording");
	}

	/**
	 * Stop recording the video. This will trigger the listener added via
	 * {@link #addVideoCompletedConsumer(SerializableConsumer)} once the last chunk
	 * has been received.
	 */
	public void stopRecording()
	{
		this.getElement().callJsFunction("stopRecording");
	}

	/**
	 * Start playing the video. If the video was paused, this will unpause the
	 * video. If the video has stopped because the end was reached, this will play
	 * from the beginning.
	 *
	 * @see #pause()
	 */
	public void play()
	{
		this.getElement().callJsFunction("play");
	}

	/**
	 * Pauses the video. Calling this method subsequently has no effects.
	 *
	 * @see #play()
	 */
	public void pause()
	{
		this.getElement().callJsFunction("pause");
	}

	/**
	 * Grab the current frame displayed in the video element. This will create a
	 * picture using the html canvas and then return to the consumers added via the
	 * {@link #addPictureConsumer(SerializableConsumer)} method.
	 */
	public void takePicture()
	{
		this.getElement().callJsFunction("takePicture");
	}

	@ClientCallable
	private void receiveRecordChunk(final JsonString data)
	{
		// The received data chunk is encoded in a base64 string
		final Decoder decoder = Base64.getDecoder();
		final String s = data.asString();
		final byte[] decodedData = decoder.decode(s.substring(s.indexOf("base64,") + 7));
		this.notifyConsumers(byte[].class, decodedData);
	}
	
	@ClientCallable
	private void onRecordStart(final JsonString data)
	{
		this.notifyConsumers(String.class, data.asString());
	}

	@ClientCallable
	public void onPictureReceived(final JsonString data)
	{
		final Decoder decoder = Base64.getDecoder();
		final byte[] decodedData = decoder.decode(data.asString().split(",")[1]);
		this.notifyConsumers(ImageWrapper.class, new ImageWrapper(decodedData));
	}
}
