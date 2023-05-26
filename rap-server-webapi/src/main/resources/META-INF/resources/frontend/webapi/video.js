import { html, LitElement } from 'lit';

class RapVideo extends LitElement {
	static get is() {
		return 'rap-video';
    }
    
    render() {
		return html`
			<video id='rapvideo' controls width="${this.videoWidth}" height="${this.videoHeight}">
				${sources.forEach(item =>
					html`<source src="${item.src}" type="${item.type}" />` 
				)}
	        </video>
		`;
	}

    addDeviceCameraVideoSource(withVideo, withAudio, withFrontCamera) {
        navigator.mediaDevices.getUserMedia({
            audio: withAudio,
            video: withVideo 
                ? { facingMode: { ideal: withFrontCamera 
                    ? "user"
                    : "environment" }}
                : false
            }).then(stream => this.$.rapvideo.srcObject = stream);
    }

    startRecording() {
        this.recorder = new MediaRecorder(this.$.rapvideo.srcObject);
        // When a chunk becomes available send it to the server
        this.recorder.ondataavailable = event => {
            let reader = new FileReader();
            reader.onloadend = () => this.$server.receiveRecordChunk(reader.result);
            reader.readAsDataURL(event.data);
        }
        // Record 5 second chunks instead of one massive blob that might not be able to be transfered to the server
        this.recorder.start(5000);
    }

    stopRecording() {
        this.recorder.stop();
        return this.recorder.mimeType;
    }
    
    play() {
    	this.$.rapvideo.play();
    }
    
    pause() {
    	this.$.rapvideo.pause();
    }

    takePicture() {
        let video = this.$.rapvideo;

        if(video.srcObject === null) {
            return;
        }

        let canvas = document.createElement('canvas');
        canvas.width = video.videoWidth;
        canvas.height = video.videoHeight;
        canvas.getContext('2d').drawImage(video, 0, 0, video.videoWidth, video.videoHeight);
        canvas.toBlob(blob => {
            let reader = new FileReader();
            reader.onloadend = () => this.$server.onPictureReceived(reader.result);
            reader.readAsDataURL(blob);
        });
    }
}

customElements.define(RapVideo.is, RapVideo);
export default RapVideo;
