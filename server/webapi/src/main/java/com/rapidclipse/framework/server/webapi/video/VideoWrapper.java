
package com.rapidclipse.framework.server.webapi.video;

import java.io.Serializable;
import java.util.List;


/**
 *
 *
 * @author XDEV Software
 * @since 10.02.00
 */
public class VideoWrapper implements Serializable
{
	private final String       mimeType;
	private final List<byte[]> data;
	
	public VideoWrapper(final String mimeType, final List<byte[]> data)
	{
		this.mimeType = mimeType;
		this.data     = data;
	}
	
	public String getMimeType()
	{
		return this.mimeType;
	}
	
	public List<byte[]> getData()
	{
		return this.data;
	}
}
