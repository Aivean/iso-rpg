// Speech bubble


var SpeechBubble = function(game, x, y, width, text) {
	Phaser.Sprite.call(this, game, x, y, 'speech-bubble', 'e');

	// Some sensible minimum defaults
	width = width || 27;
	var height = 18;

	var style = { font: "250 10px monospace", fill: "#000", align: "center"};

	var corner = game.make.image(0, 0, 'speech-bubble', 'tl');
	var cW = corner.width;
	var cH = corner.height;

	// Set up our text and run our custom wrapping routine on it
	this.bitmapText = game.make.text(cW, cH, text, style);

	this.bitmapText.wordWrap = true;
	this.bitmapText.wordWrapWidth = width;

	// Set up our text and run our custom wrapping routine on it
	//this.bitmapText = game.make.text(x + 12, y + 4, '8bitoperator', text, 10);
//	SpeechBubble.wrapBitmapText(this.bitmapText, width);

	// Calculate the width and height needed for the edges
	var bounds = this.bitmapText.getLocalBounds();
	while (bounds.width > width) {
		width = bounds.width;
		this.bitmapText.wordWrapWidth = width;
		//this.bitmapText.updateText();
		bounds = this.bitmapText.getLocalBounds();
	}
	if (bounds.width < Math.round(bounds.height * 1.1)) {
		this.bitmapText.wordWrapWidth = Math.round(bounds.height * 1.1);
		bounds = this.bitmapText.getLocalBounds();
	}


	if (bounds.width + cW * 2 > width) {
		width = bounds.width + cW * 2;
	}
	if (bounds.height + cH * 2 > height) {
		height = bounds.height + cH * 2;
	}


	this.addChild(
		game.make.tileSprite(cW, cH, width - cW * 2, height - cH * 2,
			'speech-bubble', 'c'));

	this.addChild(
		game.make.tileSprite(cW, 0, width - cW * 2, cH, 'speech-bubble', 't'));

	// Add the tail
	var tail = game.make.image(0, height - cH, 'speech-bubble', 'tail');
	tail.x = game.math.clamp(width / 2, cW, width - tail.width - cW);
	this.tail = this.addChild(tail);

	if (tail.x > cW) {
		this.addChild(
			game.make.tileSprite(cW, height - cH, tail.x - cW, cH,
				'speech-bubble', 'b'));
	}

	if (tail.x + tail.width < width - cW) {
		this.addChild(
			game.make.tileSprite(tail.x + tail.width, height - cH,
				width - cW - (tail.x + tail.width), cH, 'speech-bubble',
				'b'));
	}

	this.addChild(
		game.make.tileSprite(0, cH, cW, height - cH * 2, 'speech-bubble', 'l'));

	this.addChild(
		game.make.tileSprite(0 + width - cW,  cH, cW, height - cH * 2, 'speech-bubble', 'r'));

	this.addChild(corner);
	this.addChild(game.make.image(width - cW, 0, 'speech-bubble', 'tr'));
	this.addChild(game.make.image(0, height - cH, 'speech-bubble', 'bl'));
	this.addChild(game.make.image(width - cW, height - cH, 'speech-bubble', 'br'));

	// Add our text last so it's on top
	this.addChild(this.bitmapText);
	//this.bitmapText.tint = 0x111111;

	// Offset the position to be centered on the end of the tail
	this.pivot.set(tail.x, tail.y + tail.height);
};

SpeechBubble.prototype = Object.create(Phaser.Sprite.prototype);
SpeechBubble.prototype.constructor = SpeechBubble;

SpeechBubble.wrapBitmapText = function (bitmapText, maxWidth) {
	var words = bitmapText.text.split(' '), output = "", test = "";
	console.log(words);
	var len = words.length;
	for (var w = 0; w < len; w++) {
		test += words[w] + " ";
		bitmapText.text = test;
		bitmapText.updateText();
		console.log(test, bitmapText.width, maxWidth);
		if (bitmapText.width > maxWidth) {
			output += "\n" + words[w] + " ";
		}
		else {
			output += words[w] + " ";
		}
		test = output;
	}

	output = output.replace(/(\s)$/gm, ""); // remove trailing spaces
	bitmapText.text = output;
	bitmapText.updateText();
};
