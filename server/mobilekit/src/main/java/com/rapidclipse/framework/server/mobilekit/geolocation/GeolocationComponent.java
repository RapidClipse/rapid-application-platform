
package com.rapidclipse.framework.server.mobilekit.geolocation;

import java.util.function.Consumer;

import com.rapidclipse.framework.server.mobilekit.MobileComponent;
import com.rapidclipse.framework.server.mobilekit.geolocation.GeolocationServiceError.Reason;
import com.vaadin.flow.component.ClientCallable;
import com.vaadin.flow.component.Tag;
import com.vaadin.flow.component.dependency.HtmlImport;

import elemental.json.JsonObject;


/**
 * @author XDEV Software
 *
 */
@Tag("mobilekit-geolocation")
@HtmlImport("geolocation.html")
public class GeolocationComponent extends MobileComponent implements GeolocationService
{
	public GeolocationComponent()
	{
		super();
	}
	
	@Override
	public void getCurrentPosition(
		final GeolocationOptions options,
		final Consumer<Position> successCallback,
		final Consumer<GeolocationServiceError> errorCallback)
	{
		final String id = registerCall(successCallback, errorCallback);
		getElement().callFunction("getCurrentPosition", id, toJson(options));
	}
	
	@ClientCallable
	void getCurrentPosition_success(final String id, final JsonObject positionObj)
	{
		final Position position = toJava(positionObj, PositionImpl.class);
		getAndRemoveCall(id).success(position);
	}
	
	@ClientCallable
	void getCurrentPosition_error(final String id, final JsonObject errorObj)
	{
		final GeolocationServiceError error = toGeolocationServiceError(errorObj);
		getAndRemoveCall(id).error(error);
	}
	
	@Override
	public void watchPosition(
		final GeolocationOptions options,
		final Consumer<PositionWatch> successCallback,
		final Consumer<GeolocationServiceError> errorCallback)
	{
		final String id = registerCall(successCallback, errorCallback);
		getElement().callFunction("watchPosition", id, toJson(options));
	}
	
	@ClientCallable
	void watchPosition_success(final String id, final JsonObject positionObj, final String watchId)
	{
		final Position      position = toJava(positionObj, PositionImpl.class);
		final PositionWatch watch    = new PositionWatchImpl(position, id, watchId);
		getCall(id).success(watch);
	}
	
	@ClientCallable
	void watchPosition_error(final String id, final JsonObject errorObj)
	{
		final GeolocationServiceError error = toGeolocationServiceError(errorObj);
		getAndRemoveCall(id).error(error);
	}
	
	private void clearWatch(final String id, final String watchId)
	{
		removeCall(id);
		getElement().callFunction("clearWatch", watchId);
	}
	
	private GeolocationServiceError toGeolocationServiceError(final JsonObject error)
	{
		String message = "";
		Reason reason  = null;
		
		try
		{
			message = error.getString("message");
		}
		catch(final Exception e)
		{
			// swallow
		}
		
		try
		{
			reason = Reason.getByCode((int)error.getNumber("code"));
		}
		catch(final Exception e)
		{
			// swallow
		}
		
		return new GeolocationServiceError(this, message, reason);
	}
	
	private static class CoordinatesImpl implements Coordinates
	{
		private final double latitude;
		private final double longitude;
		private final double altitude;
		private final double accuracy;
		private final double altitudeAccuracy;
		private final double heading;
		private final double speed;
		
		@SuppressWarnings("unused") // Used by Gson via reflection
		CoordinatesImpl(
			final double latitude,
			final double longitude,
			final double altitude,
			final double accuracy,
			final double altitudeAccuracy,
			final double heading,
			final double speed)
		{
			super();
			this.latitude         = latitude;
			this.longitude        = longitude;
			this.altitude         = altitude;
			this.accuracy         = accuracy;
			this.altitudeAccuracy = altitudeAccuracy;
			this.heading          = heading;
			this.speed            = speed;
		}
		
		@Override
		public double getLatitude()
		{
			return this.latitude;
		}
		
		@Override
		public double getLongitude()
		{
			return this.longitude;
		}
		
		@Override
		public double getAltitude()
		{
			return this.altitude;
		}
		
		@Override
		public double getAccuracy()
		{
			return this.accuracy;
		}
		
		@Override
		public double getAltitudeAccuracy()
		{
			return this.altitudeAccuracy;
		}
		
		@Override
		public double getHeading()
		{
			return this.heading;
		}
		
		@Override
		public double getSpeed()
		{
			return this.speed;
		}
		
		@Override
		public String toString()
		{
			return "Coordinates [latitude=" + this.latitude + ", longitude=" + this.longitude
				+ ", altitude=" + this.altitude + ", accuracy=" + this.accuracy
				+ ", altitudeAccuracy=" + this.altitudeAccuracy + ", heading=" + this.heading
				+ ", speed=" + this.speed + "]";
		}
	}
	
	private static class PositionImpl implements Position
	{
		private final CoordinatesImpl coords;
		private final long            timestamp;
		
		@SuppressWarnings("unused") // Used by Gson via reflection
		PositionImpl(final CoordinatesImpl coords, final long timestamp)
		{
			super();
			this.coords    = coords;
			this.timestamp = timestamp;
		}
		
		@Override
		public Coordinates getCoordinates()
		{
			return this.coords;
		}
		
		@Override
		public long getTimestamp()
		{
			return this.timestamp;
		}
		
		@Override
		public String toString()
		{
			return "Position [coordinates=" + this.coords + ", timestamp=" + this.timestamp + "]";
		}
	}
	
	private class PositionWatchImpl implements PositionWatch
	{
		private final Position position;
		private final String   id;
		private final String   watchId;
		
		PositionWatchImpl(final Position position, final String id, final String watchId)
		{
			super();
			this.position = position;
			this.id       = id;
			this.watchId  = watchId;
		}
		
		@Override
		public Position getPosition()
		{
			return this.position;
		}
		
		@Override
		public void remove()
		{
			GeolocationComponent.this.clearWatch(this.id, this.watchId);
		}
		
		@Override
		public String toString()
		{
			return "PositionWatch [position=" + this.position + "]";
		}
	}
}
