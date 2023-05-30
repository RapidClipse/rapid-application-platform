import { LitElement, html } from 'lit';
import { customElement } from 'lit/decorators.js';

interface ScreenOrientationDto {
    type: string,
    angle: number
}

@customElement('rap-screen')
export class RapScreen extends LitElement {
    
	// Required so api users don't get warning spam in their logs
	render() {
		return html``;
	}

	requestFullscreen(target: HTMLElement, options?: { navigationalUi?: string}) {
        target.requestFullscreen(options).catch(e => this.$server.onFullscreenFailed(this.cloneErrorObject(e)));
    }

    exitFullscreen() {
        document.exitFullscreen().catch(e => this.$server.onFullscreenExitFailed(this.cloneErrorObject(e)));
    }

    registerOnScreenOrientationChangeListener() {
        screen.orientation.onchange = _e => this.$server.onScreenOrientationChange(this.getOrientationObject());
    }

    unregisterOnScreenOrientationChangeListener() {
        screen.orientation.onchange = null;
    }

    // Copy over values as for some reason the server would only receive empty fields
	private getOrientationObject(): ScreenOrientationDto {
        const o = screen.orientation;
        const retval = {
            type: o.type,
            angle: o.angle
        };
        return retval;
	}
    
	// Copy over values as for some reason the server would only receive empty fields
	private cloneErrorObject(e: Error) {
		const cloned = { name: e.name, message: e.message };
		return cloned;
	}
}
