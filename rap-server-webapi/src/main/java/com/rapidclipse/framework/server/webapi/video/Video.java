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
import java.util.LinkedList;
import java.util.List;

import com.rapidclipse.framework.server.webapi.JavascriptTemplate;
import com.vaadin.flow.component.ClientCallable;
import com.vaadin.flow.component.HasSize;
import com.vaadin.flow.component.Tag;
import com.vaadin.flow.component.dependency.HtmlImport;
import com.vaadin.flow.component.polymertemplate.RepeatIndex;
import com.vaadin.flow.function.SerializableConsumer;
import com.vaadin.flow.shared.Registration;
import com.vaadin.flow.templatemodel.TemplateModel;

import elemental.json.JsonString;


/**
 * The html video element that can contain multimedia such as videos. To add such sources the {@link #addSource(Source)}
 * method can be used. To add the users device camera the {@link #addDeviceCameraVideoSource(boolean, boolean, boolean)}
 * method can be used. You can also take pictures with the {@link #takePicture()} method.
 *
 * @author XDEV Software
 * @since 10.02.00
 */
@HtmlImport("frontend://webapi/video.html")
@Tag("rap-video")
public class Video extends JavascriptTemplate<Video.VideoTemplateModel> implements HasSize
{
	// TODO: What to do if no more memory is left? Maybe add some sort of immediate mode, where the server
	// can immediately process the received video after receiving one of the chunks
	private final List<byte[]> recordedData = new LinkedList<>();
	private String             mimeType     = null;
	private boolean            isRecording  = false;
	
	public Video()
	{
		this.setSizeFull();
	}
	
	/**
	 * Add a consumer for the video received event triggered by {@link #stopRecording()}. This consumer will be called
	 * when the video transmission has ended.
	 */
	public Registration addVideoConsumer(final SerializableConsumer<VideoWrapper> consumer)
	{
		return this.registerConsumer(VideoWrapper.class, consumer);
	}
	
	/**
	 * Add a consumer for the picture received event triggered by {@link #takePicture()}. It will consume the image
	 * received. The result is wrapped in an ImageWrapper that contains convinience methods such as
	 * {@link ImageWrapper#toStreamResource(String)}
	 */
	public Registration addPictureConsumer(final SerializableConsumer<ImageWrapper> consumer)
	{
		return this.registerConsumer(ImageWrapper.class, consumer);
	}
	
	/**
	 * Set the list of sources for the video element.
	 */
	public Video setSources(final List<Source> sources)
	{
		this.getModel().setSources(sources);
		return this;
	}
	
	/**
	 * Add a source to the list of sources for the video element. These sources are usually the same video but in
	 * different formats to support as many browsers as possible.
	 */
	public void addSource(final Source source)
	{
		this.getModel().getSources().add(source);
	}
	
	/**
	 * Add the users device camera as a source.
	 */
	public void addDeviceCameraVideoSource(
		final boolean withVideo,
		final boolean useFrontCamera,
		final boolean withAudio)
	{
		this.getElement().callJsFunction("addDeviceCameraVideoSource", withVideo, withAudio, useFrontCamera);
	}
	
	/**
	 * Remove a source with the given index.
	 */
	public void removeSource(@RepeatIndex final int index)
	{
		this.getModel().getSources().remove(index);
	}
	
	public List<Source> getSources()
	{
		return this.getModel().getSources();
	}
	
	/**
	 * Start recording the current source.
	 */
	public void startRecording()
	{
		this.isRecording = true;
		this.getElement().callJsFunction("startRecording");
	}
	
	/**
	 * Stop recording the current source and return the result to the consumers added via the
	 * {@link #addVideoConsumer(SerializableConsumer)} method.
	 */
	public void stopRecording()
	{
		this.isRecording = false;
		this.getElement().callJsFunction("stopRecording")
			.then(String.class, mimeType -> this.mimeType = mimeType);
	}
	
	/**
	 * Start playing the video. If the video was paused, this will unpause the video. If the video has stopped because
	 * the end was reached, this will play from the beginning.
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
	 * Grab the current frame displayed in the video element. This will create a picture using the html canvas and then
	 * return to the consumers added via the {@link #addPictureConsumer(SerializableConsumer)} method.
	 */
	public void takePicture()
	{
		this.getElement().callJsFunction("takePicture");
	}
	
	@Override
	public void setWidth(final String width)
	{
		HasSize.super.setWidth(width);
		this.getModel().setVideoWidth(width);
	}
	
	@Override
	public void setWidthFull()
	{
		HasSize.super.setWidthFull();
		this.getModel().setVideoWidth("100%");
	}
	
	@Override
	public void setHeight(final String height)
	{
		HasSize.super.setHeight(height);
		this.getModel().setVideoHeight(height);
	}
	
	@Override
	public void setHeightFull()
	{
		HasSize.super.setHeightFull();
		this.getModel().setVideoHeight("100%");
	}
	
	@ClientCallable
	private void receiveRecordChunk(final JsonString data)
	{
		// The received data chunk is encoded in a base64 string
		final Decoder decoder     = Base64.getDecoder();
		final String  s           = data.asString();
		final byte[]  decodedData = decoder.decode(s.substring(s.indexOf("base64,") + 7));
		this.recordedData.add(decodedData);
		
		// If the recording was stopped and this is the last data chunk we will receive, notify the consumers
		if(!this.isRecording)
		{
			this.notifyConsumers(VideoWrapper.class, new VideoWrapper(this.mimeType, this.recordedData));
		}
	}
	
	@ClientCallable
	public void onPictureReceived(final JsonString data)
	{
		final Decoder decoder     = Base64.getDecoder();
		final byte[]  decodedData = decoder.decode(data.asString().split(",")[1]);
		this.notifyConsumers(ImageWrapper.class, new ImageWrapper(decodedData));
	}
	
	public interface VideoTemplateModel extends TemplateModel
	{
		void setSources(final List<Source> sources);
		
		List<Source> getSources();
		
		void setVideoWidth(final String width);
		
		void setVideoHeight(final String height);
	}
}
