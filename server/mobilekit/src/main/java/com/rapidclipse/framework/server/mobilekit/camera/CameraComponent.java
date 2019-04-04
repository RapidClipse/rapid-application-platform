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
package com.rapidclipse.framework.server.mobilekit.camera;

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.util.Base64;
import java.util.function.Consumer;

import com.rapidclipse.framework.server.mobilekit.MobileComponent;
import com.rapidclipse.framework.server.mobilekit.MobileServiceError;
import com.vaadin.flow.component.ClientCallable;
import com.vaadin.flow.component.Tag;
import com.vaadin.flow.component.dependency.HtmlImport;
import com.vaadin.flow.server.AbstractStreamResource;
import com.vaadin.flow.server.StreamResource;


/**
 * @author XDEV Software
 *
 */
@Tag("mobilekit-camera")
@HtmlImport("camera.html")
public class CameraComponent extends MobileComponent implements CameraService
{
	public CameraComponent()
	{
		super();
	}
	
	@Override
	public void getPicture(
		final CameraOptions options,
		final Consumer<Picture> successCallback,
		final Consumer<MobileServiceError> errorCallback)
	{
		final String id = registerCall(new GetPictureServiceCall(successCallback, errorCallback, options));
		getElement().callFunction("getPicture", id, toJson(options));
	}
	
	@ClientCallable
	void getPicture_success(final String id, final String pictureData)
	{
		final GetPictureServiceCall call    = getAndRemoveCall(id);
		final Picture               picture = new PictureImpl(call.options, pictureData);
		call.success(picture);
	}
	
	@ClientCallable
	void getPicture_error(final String id, final String errorMessage)
	{
		final MobileServiceError error = new MobileServiceError(this, errorMessage);
		getAndRemoveCall(id).error(error);
	}
	
	private static class GetPictureServiceCall
		extends ServiceCall.Implementation<Picture, MobileServiceError>
	{
		final CameraOptions options;
		
		GetPictureServiceCall(
			final Consumer<Picture> successCallback,
			final Consumer<MobileServiceError> errorCallback,
			final CameraOptions options)
		{
			super(successCallback, errorCallback);
			this.options = options;
		}
	}
	
	private static class PictureImpl implements Picture
	{
		private String base64data;
		private String uri;
		
		public PictureImpl(final CameraOptions options, final String value)
		{
			if(options.getDestinationType() == DestinationType.IMAGE)
			{
				this.base64data = value;
			}
			else
			{
				this.uri = value;
			}
		}
		
		@Override
		public String getBase64data()
		{
			return this.base64data;
		}
		
		@Override
		public byte[] toRawData()
		{
			if(this.base64data == null)
			{
				throw new IllegalArgumentException("Picture contains only URI");
			}
			
			return Base64.getDecoder().decode(this.base64data);
		}
		
		@Override
		public InputStream toInputStream()
		{
			return new ByteArrayInputStream(toRawData());
		}
		
		@Override
		public AbstractStreamResource toResource()
		{
			if(this.base64data == null)
			{
				throw new IllegalArgumentException("ImageData contains only URI");
			}
			
			return new StreamResource("MobilekitPicture", this::toInputStream);
		}
		
		@Override
		public String getURI()
		{
			return this.uri;
		}
	}
}
