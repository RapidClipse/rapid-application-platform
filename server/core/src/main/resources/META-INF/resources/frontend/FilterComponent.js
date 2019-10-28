window.onload = function(){
	main();


}

function main(){

	alert("main");
	var combobox = document.getElementsByTagName("vaadin-combo-box");
	for(var i = 0; i< combobox.length; i++){
		alert("forloop");
		combobox[i].shadowRoot.innerHTML = <style>
				background-color = blue;

		</style>
	}
}