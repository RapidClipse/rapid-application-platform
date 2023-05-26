import { LitElement } from 'lit';

class RapTouch extends LitElement {
	static get is() {
		return 'rap-touch';
	}

	cloneTouchArray(arr) {
		let retval = [];
		for(let i = 0; i < arr.length; i++) {
			let el = arr[i];
			retval.push({
				identifier: el.identifier,
				screenX: el.screenX,
				screenY: el.screenY,
				clientX: el.clientX,
				clientY: el.clientY,
				pageX: el.pageX,
				pageY: el.pageY,
				radiusX: el.radiusX,
				radiusY: el.radiusY,
				rotationAngle: el.rotationAngle,
				force: el.force
			});
		}
		return retval;
	}

	// For some reason we have to copy the values for them to get loaded
	cloneTouchEvent(ev) {
		let retval = {
			altKey: ev.altKey,
			changedTouches: this.cloneTouchArray(ev.changedTouches),
			ctrlKey: ev.ctrlKey,
			metaKey: ev.metaKey,
			shiftKey: ev.shiftKey,
			targetTouches: this.cloneTouchArray(ev.targetTouches),
			touches: this.cloneTouchArray(ev.touches)
		};

		return retval;
	}
	
	registerTouchListener(target) {
		target.ontouchstart = e => this.$server.onTouchStart(this.cloneTouchEvent(e));
		target.ontouchend = e => this.$server.onTouchEnd(this.cloneTouchEvent(e));
		target.ontouchmove = e => this.$server.onTouchMove(this.cloneTouchEvent(e));
		target.ontouchcancel = e => this.$server.onTouchCancel(this.cloneTouchEvent(e));
	}
	
	unregisterTouchListener(target) {
		target.ontouchstart = undefined;
		target.ontouchend = undefined;
		target.ontouchmove = undefined;
		target.ontouchcancel = undefined;
	}
}

customElements.define(RapTouch.is, RapTouch);
export default RapTouch;
