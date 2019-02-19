
package com.rapidclipse.framework.server.mobilekit.geolocation;

import com.rapidclipse.framework.server.mobilekit.MobileServiceError;


/**
 * @author XDEV Software
 *
 */
public class GeolocationServiceError extends MobileServiceError
{
	public static enum Reason
	{
		/**
		 * Returned when users do not allow the app to retrieve position
		 * information. This is dependent on the platform.
		 */
		PERMISSION_DENIED(1),

		/**
		 * Returned when the device is unable to retrieve a position. In
		 * general, this means the device is not connected to a network or can't
		 * get a satellite fix.
		 */
		POSITION_UNAVAILABLE(2),

		/**
		 * Returned when the device is unable to retrieve a position within the
		 * time specified by the timeout included in {@link GeolocationOptions}.
		 * When used with
		 * {@link GeolocationService#watchPosition(java.util.function.Consumer, java.util.function.Consumer, int)},
		 * this error could be repeatedly passed to the error callback every
		 * timeout milliseconds.
		 */
		TIMEOUT(3);

		private final int code;
		
		private Reason(final int code)
		{
			this.code = code;
		}
		
		public static Reason getByCode(final int code)
		{
			for(final Reason reason : values())
			{
				if(reason.code == code)
				{
					return reason;
				}
			}
			return null;
		}
	}

	private final Reason reason;
	
	public GeolocationServiceError(
		final GeolocationService source,
		final String message,
		final Reason reason)
	{
		super(source, message);

		this.reason = reason;
	}
	
	@Override
	public GeolocationService getSource()
	{
		return (GeolocationService)super.getSource();
	}
	
	public Reason getReason()
	{
		return this.reason;
	}
}
