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

package com.rapidclipse.framework.server.mobilekit.file;

import java.io.ByteArrayInputStream;
import java.util.function.Consumer;

import org.apache.commons.codec.binary.Base64;

import com.rapidclipse.framework.server.mobilekit.MobileComponent;
import com.rapidclipse.framework.server.mobilekit.file.FileServiceError.Reason;
import com.vaadin.flow.component.ClientCallable;
import com.vaadin.flow.component.Tag;
import com.vaadin.flow.component.dependency.HtmlImport;
import com.vaadin.flow.server.AbstractStreamResource;
import com.vaadin.flow.server.StreamResource;

import elemental.json.JsonObject;
import elemental.json.JsonValue;


/**
 * @author XDEV Software
 *
 */
@Tag("mobilekit-file")
@HtmlImport("file.html")
public class FileComponent extends MobileComponent implements FileService
{
	public FileComponent()
	{
		super();
	}

	@Override
	public void readFile(
		final String path,
		final Consumer<FileData> successCallback,
		final Consumer<FileServiceError> errorCallback)
	{
		final String id = registerCall(successCallback, errorCallback);
		getElement().callJsFunction("readFile", id, path);
	}

	@ClientCallable
	void readFile_success(
		final String id,
		final JsonObject fileDataObj)
	{
		final FileData fileData = toJava(fileDataObj, FileDataImpl.class);
		getAndRemoveCall(id).success(fileData);
	}

	@ClientCallable
	void readFile_error(final String id, final JsonValue errorValue)
	{
		final FileServiceError error = createFileServiceError("Error reading file", errorValue);
		getAndRemoveCall(id).error(error);
	}

	private FileServiceError createFileServiceError(final String message, final JsonValue value)
	{
		Reason reason = null;
		if(value instanceof JsonObject)
		{
			try
			{
				final int code = (int)((JsonObject)value).getNumber("code");
				reason = Reason.getByCode(code);
			}
			catch(final Exception e)
			{
				// swallow
			}
		}
		return new FileServiceError(this, message, reason);
	}

	private static class FileDataImpl implements FileData
	{
		private final String base64data;

		@SuppressWarnings("unused") // Used by Gson via reflection
		FileDataImpl(final String base64data)
		{
			this.base64data = base64data;
		}

		@Override
		public String getBase64data()
		{
			return this.base64data;
		}

		@Override
		public byte[] toRawData()
		{
			final String encodingPrefix    = "base64,";
			final int    contentStartIndex = this.base64data.indexOf(encodingPrefix)
				+ encodingPrefix.length();
			final String data              = contentStartIndex == -1 ? this.base64data
				: this.base64data.substring(contentStartIndex);
			return new Base64(true).decode(data);
		}

		@Override
		public AbstractStreamResource toResource()
		{
			return new StreamResource(toString(), () -> new ByteArrayInputStream(toRawData()));
		}
	}
}
