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

/**
 * Optional parameters to customize the camera settings
 *
 * @author XDEV Software
 *
 */
public class CameraOptions
{
	public static CameraOptions takeAndReturnPicture()
	{
		return new CameraOptions().mediaType(MediaType.PICTURE)
			.destinationType(DestinationType.IMAGE).correctOrientation();
	}
	
	private int               quality            = 50;
	private DestinationType   destinationType    = DestinationType.FILE_URI;
	private PictureSourceType sourceType         = PictureSourceType.CAMERA;
	private final boolean     allowEdit          = false;
	private EncodingType      encodingType       = EncodingType.JPEG;
	private Integer           targetWidth        = null;
	private Integer           targetHeight       = null;
	private MediaType         mediaType          = MediaType.PICTURE;
	private boolean           correctOrientation = false;
	private boolean           saveToPhotoAlbum   = false;
	private Direction         direction          = Direction.BACK;

	public CameraOptions()
	{
	}

	public int getQuality()
	{
		return this.quality;
	}

	/**
	 * Quality of the saved image, expressed as a range of 0-100, where 100 is
	 * typically full resolution with no loss from file compression. (Note that
	 * information about the camera's resolution is unavailable.)
	 */
	public CameraOptions quality(final int quality)
	{
		this.quality = quality;
		return this;
	}

	/**
	 * Choose the format of the return value.
	 */
	public CameraOptions destinationType(final DestinationType destinationType)
	{
		this.destinationType = destinationType;
		return this;
	}

	/**
	 * Set the source of the picture.
	 */
	public CameraOptions sourceType(final PictureSourceType sourceType)
	{
		this.sourceType = sourceType;
		return this;
	}

	/*
	 * https://github.com/apache/cordova-plugin-camera#android-quirks-1
	 */
	// public CameraOptions allowEdit()
	// {
	// this.allowEdit = true;
	// return this;
	// }
	
	/**
	 * Choose the returned image file's encoding.
	 */
	public CameraOptions encodingType(final EncodingType encodingType)
	{
		this.encodingType = encodingType;
		return this;
	}

	/**
	 * Width and height in pixels to scale image.
	 */
	public CameraOptions targetSize(final int width, final int height)
	{
		this.targetWidth  = width;
		this.targetHeight = height;
		return this;
	}

	/**
	 * Set the type of media to select from. Only works when {@link #sourceType}
	 * is {@link PictureSourceType#PHOTO_LIBRARY} or
	 * {@link PictureSourceType#SAVED_PHOTO_ALBUM}.
	 */
	public CameraOptions mediaType(final MediaType mediaType)
	{
		this.mediaType = mediaType;
		return this;
	}

	/**
	 * Rotate the image to correct for the orientation of the device during
	 * capture.
	 */
	public CameraOptions correctOrientation()
	{
		this.correctOrientation = true;
		return this;
	}

	/**
	 * Save the image to the photo album on the device after capture.
	 */
	public CameraOptions saveToPhotoAlbum()
	{
		this.saveToPhotoAlbum = true;
		return this;
	}

	/**
	 * Choose the camera to use (front- or back-facing).
	 */
	public CameraOptions direction(final Direction direction)
	{
		this.direction = direction;
		return this;
	}

	/**
	 * @return the destination type setting
	 * @see #destinationType(DestinationType)
	 */
	public DestinationType getDestinationType()
	{
		return this.destinationType;
	}

	/**
	 * @return the source type setting
	 * @see #sourceType(PictureSourceType)
	 */
	public PictureSourceType getSourceType()
	{
		return this.sourceType;
	}

	/**
	 * @return the allow edit setting
	 * @see #isAllowEdit()
	 */
	public boolean isAllowEdit()
	{
		return this.allowEdit;
	}

	/**
	 * @return the encoding type setting
	 * @see #encodingType(EncodingType)
	 */
	public EncodingType getEncodingType()
	{
		return this.encodingType;
	}

	/**
	 * @return the target width setting
	 * @see #targetSize(int, int)
	 */
	public Integer getTargetWidth()
	{
		return this.targetWidth;
	}

	/**
	 * @return the target height setting
	 * @see #targetSize(int, int)
	 */
	public Integer getTargetHeight()
	{
		return this.targetHeight;
	}

	/**
	 * @return the media type setting
	 * @see #mediaType(MediaType)
	 */
	public MediaType getMediaType()
	{
		return this.mediaType;
	}

	/**
	 * @return the correct orientation setting
	 * @see #correctOrientation()
	 */
	public boolean isCorrectOrientation()
	{
		return this.correctOrientation;
	}

	/**
	 * @return the save to photo album setting
	 * @see #saveToPhotoAlbum()
	 */
	public boolean isSaveToPhotoAlbum()
	{
		return this.saveToPhotoAlbum;
	}

	/**
	 * @return the direction setting
	 * @see #direction(Direction)
	 */
	public Direction getDirection()
	{
		return this.direction;
	}
}
