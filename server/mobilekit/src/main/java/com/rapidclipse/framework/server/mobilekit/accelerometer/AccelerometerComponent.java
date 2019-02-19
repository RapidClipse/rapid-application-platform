
package com.rapidclipse.framework.server.mobilekit.accelerometer;

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
@Tag("mobilekit-accelerometer")
@HtmlImport("accelerometer.html")
public class AccelerometerComponent extends MobileComponent implements AccelerometerService
{
	public AccelerometerComponent()
	{
		super();
	}
	
	@Override
	public void getCurrentAcceleration(
		final AccelerometerOptions options,
		final Consumer<Acceleration> successCallback,
		final Consumer<MobileServiceError> errorCallback)
	{
		final String id = registerCall(successCallback, errorCallback);
		getElement().callFunction("getCurrentAcceleration", id, toJson(options));
	}
	
	@ClientCallable
	void getCurrentAcceleration_success(final String id, final JsonObject positionObj)
	{
		final Acceleration acceleration = toJava(positionObj, AccelerationImpl.class);
		getAndRemoveCall(id).success(acceleration);
	}
	
	@ClientCallable
	void getCurrentAcceleration_error(final String id, final String errorMessage)
	{
		final MobileServiceError error = new MobileServiceError(this, errorMessage);
		getAndRemoveCall(id).error(error);
	}
	
	@Override
	public void watchAcceleration(
		final AccelerometerOptions options,
		final Consumer<AccelerationWatch> successCallback,
		final Consumer<MobileServiceError> errorCallback)
	{
		final String id = registerCall(successCallback, errorCallback);
		getElement().callFunction("watchAcceleration", id, toJson(options));
	}
	
	@ClientCallable
	void watchAcceleration_success(
		final String id,
		final JsonObject positionObj,
		final String watchId)
	{
		final Acceleration      acceleration = toJava(positionObj, AccelerationImpl.class);
		final AccelerationWatch watch        = new AccelerationWatchImpl(acceleration, id, watchId);
		getCall(id).success(watch);
	}
	
	@ClientCallable
	void watchAcceleration_error(final String id, final String errorMessage)
	{
		final MobileServiceError error = new MobileServiceError(this, errorMessage);
		getAndRemoveCall(id).error(error);
	}
	
	private void clearWatch(final String id, final String watchId)
	{
		removeCall(id);
		getElement().callFunction("clearWatch", watchId);
	}
	
	private static class AccelerationImpl implements Acceleration
	{
		private final double x;
		private final double y;
		private final double z;
		private final long   timestamp;
		
		@SuppressWarnings("unused") // Used by Gson via reflection
		AccelerationImpl(final double x, final double y, final double z, final long timestamp)
		{
			super();
			this.x         = x;
			this.y         = y;
			this.z         = z;
			this.timestamp = timestamp;
		}
		
		@Override
		public double getX()
		{
			return this.x;
		}
		
		@Override
		public double getY()
		{
			return this.y;
		}
		
		@Override
		public double getZ()
		{
			return this.z;
		}
		
		@Override
		public long getTimestamp()
		{
			return this.timestamp;
		}
		
		@Override
		public String toString()
		{
			return "Acceleration [x=" + this.x + ", y=" + this.y + ", z=" + this.z + ", timestamp="
				+ this.timestamp + "]";
		}
	}
	
	private class AccelerationWatchImpl implements AccelerationWatch
	{
		private final Acceleration acceleration;
		private final String       id;
		private final String       watchId;
		
		AccelerationWatchImpl(
			final Acceleration Acceleration,
			final String id,
			final String watchId)
		{
			super();
			this.acceleration = Acceleration;
			this.id           = id;
			this.watchId      = watchId;
		}
		
		@Override
		public Acceleration getAcceleration()
		{
			return this.acceleration;
		}
		
		@Override
		public void remove()
		{
			AccelerometerComponent.this.clearWatch(this.id, this.watchId);
		}
		
		@Override
		public String toString()
		{
			return "AccelerationWatch [acceleration=" + this.acceleration + "]";
		}
	}
}
