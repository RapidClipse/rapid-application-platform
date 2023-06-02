import { LitElement, html } from 'lit';
import { customElement } from 'lit/decorators.js';

@customElement('rap-geolocation')
export class RapGeolocation extends LitElement {
	
	private watchId?: number;
	
	// Required so api users don't get warning spam in their logs
	render() {
		return html``;
	}
	
	watchPosition(options?: PositionOptions) {
		this.watchId = navigator.geolocation.watchPosition(
			e => this.$server.onSuccess(this.clonePositionObject(e)),
			e => this.$server.onError(this.cloneErrorObject(e)),
			options);
	}
	
	clearWatch() {
		if(this.watchId !== undefined) {
			navigator.geolocation.clearWatch(this.watchId!);
			this.watchId = undefined;
		}
	}
	
	getCurrentPosition(options?: PositionOptions) {
		navigator.geolocation.getCurrentPosition(
			e => this.$server.onSuccess(this.clonePositionObject(e)),
			e => this.$server.onError(this.cloneErrorObject(e)),
			options);
	}
	
	// Copy over values as for some reason the server would only receive empty fields
	private clonePositionObject(p: GeolocationPosition): GeolocationPosition {
		let c = p.coords;
		const coords = {
			latitude: c.latitude,
			longitude: c.longitude,
			altitude: c.altitude,
			accuracy: c.accuracy,
			altitudeAccuracy: c.altitudeAccuracy,
			heading: c.heading,
			speed: c.speed
		};
		const cloned = { timestamp: p.timestamp, coords };
		return cloned;
	}
	
	// Copy over values as for some reason the server would only receive empty fields
	private cloneErrorObject(e: GeolocationPositionError) {
		const cloned = { code: e.code, message: e.message };
		return cloned;
	}
}
