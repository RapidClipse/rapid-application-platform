import { LitElement } from 'lit';

class RapScreen extends LitElement {
	static get is() {
		return 'rap-screen';
	}

	requestFullscreen(target, options) {
        target.requestFullscreen(options)
            .catch(err => {
                const retval = {
                    message: err.message,
                    name: err.name
                };
                this.$server.onFullscreenFailed(retval);
            });
    }

    exitFullscreen() {
        document.exitFullscreen()
            .catch(err => {
                const retval = {
                    message: err.message,
                    name: err.name
                };
                this.$server.onFullscreenExitFailed(retval);
            });
    }

    registerOnScreenOrientationChangeListener() {
        screen.orientation.onchange = e => {
            const t = e.currentTarget;
            const retval = {
                type: t.type,
                angle: t.angle
            };
            this.$server.onScreenOrientationChange(retval);
        };
    }

    unregisterOnScreenOrientationChangeListener() {
        screen.orientation.onchange = undefined;
    }
}

customElements.define(RapScreen.is, RapScreen);
export default RapScreen;
