//HELPERS

function byteStringToByteArray(byteString) {
	var byteArray = new Uint8Array(byteString.length);
	for(var index = 0; index < byteString.length; index++)
		byteArray[index] = byteString.charCodeAt(index);
	byteArray.head = 0;
	return byteArray;
}

function iterate(iterable, monad) {
	if (!iterable)
		return;
	for (var i = 0; i < iterable.length; i++)
		monad(iterable[i]);
}

function byteArrayToByteString(byteArray) {
	var retval = "";
	iterate(byteArray, function(byte) { retval += String.fromCharCode(byte); });
	return retval;
}

function deflate(byteString) {
	return pako.deflateRaw(new TextEncoder().encode(byteString), {"level": 9});
}

function inflate(byteString) {
	return byteArrayToByteString(pako.inflateRaw(byteString));
}

function bufferToHex(buffer) {
	var dataView = new DataView(buffer);
	var retval = "";

	for (var i = 0; i < dataView.byteLength; i++)
		retval += (256 | dataView.getUint8(i)).toString(16).slice(-2);

	return retval;
}

function getRandomBits(minBits) {
	var crypto = window.crypto || window.msCrypto;
	return bufferToHex(crypto.getRandomValues(new Uint8Array(minBits + 7 >> 3)).buffer);
}

function streamToString(stream){
	const chunks = []
	return new Promise((resolve, reject) => {
	  	stream.on('data', chunk => chunks.push(chunk))
	  	stream.on('error', reject)
	  	stream.on('end', () => resolve(Buffer.concat(chunks).toString('utf8')))
	})
}

//END HELPERS

//CODE GENERATION

let outerHeader = `Vlang\u00001\u0000mathematica\u0000F.code.tio\u0000`
let header = null;
let footer = null;
let outerFooter = "F.input.tio\u00000\u0000Vargs\u00000\u0000R";

function getInnerString(ft, stars, levelExtraExponents, unlocked){
	return header + ft.toString() + "," + stars.toString() + "," + levelExtraExponents.toString() + "," + unlocked.toString() + footer;
}

function getFinalString(ft, stars, levelExtraExponents, unlocked){
	let innerString = getInnerString(ft, stars, levelExtraExponents, unlocked);
	// the tio format declares the length of the following code in BYTES
	let innerBytes = new TextEncoder().encode(innerString);
	return outerHeader + innerBytes.length + "\u0000" + innerString + outerFooter;
}

//END CODE GENERATION

//API

let URL_RUN = "https://tio.run/cgi-bin/static/fb67788fd3d1ebf92e66b295525335af-run/";

function processRequest(request, resolve, reject){
	if (request.readyState != XMLHttpRequest.DONE)
		return;
	var statusCode = request.status;
	var response = byteArrayToByteString(new Uint8Array(request.response));
	request = undefined;

	if (statusCode == 204) {
		reject("内容为空");
        return;
	}

	if (statusCode >= 400) {
		reject("错误 " + statusCode);
		return;
	}

	try {
		var rawOutput = inflate(response.slice(10));
	} catch(error) {
		reject("响应错误");
		return;
	}

	try {
		response = byteStringToText(rawOutput);
	} catch(error) {
		response = rawOutput;
	}

	if (response.length < 32) {
		reject("响应错误");
		return;
	}

	var results = response.substr(16).split(response.substr(0, 16));
    resolve(results[0]);
}

async function optimalStars(ft, stars, levelExtraExponents, unlocked){
    let date1 = new Date();
    if(header === null){
        let calculatordata = await fetch("Calculator2.wls");
        if (!calculatordata.ok) throw "";
        let calculatortxt = await calculatordata.text();
        let headertxt = "calcoptBLPWrapper[\n"
		if(calculatortxt === null || headertxt === null) throw "";
        header = calculatortxt.substring(calculatortxt.indexOf("\n") + 1) + headertxt;
    }
    
    if(footer === null){
        footer = "]";
    }
    
    let date2 = new Date();
    console.log(date2.getTime() - date1.getTime());
    
    return new Promise(function (resolve, reject) {
        let runRequest = new XMLHttpRequest();
        let token = getRandomBits(128);
        let finalUrl = URL_RUN + token;
        let finalBodyData = deflate(getFinalString(ft, stars, levelExtraExponents, unlocked));
        runRequest.open("POST", finalUrl, true);
        runRequest.responseType = "arraybuffer";
        runRequest.onreadystatechange = () => { processRequest(runRequest, resolve, reject); };
        runRequest.send(finalBodyData);        
    });
}

//END API
