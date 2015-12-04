/**
 * Created by Aivean on 12/3/15.
 */

importScripts('harmony-collections.min.js', 'rbush.js', 'queue.js');

var map = new Map()
var rTree = rbush(16);
var currentFlag = true;
var buffersPool = null;

var updated = new Map();
var removed = [];
var sortScheduled = false;

function isBefore(a, b) {
	var aBounds = a.bounds;
	var bBounds = b.bounds;

	var aBeforeB = (bBounds.frontX <= aBounds.backX ||
	bBounds.frontY <= aBounds.backY ||
	bBounds.top <= aBounds.bottom );

	var bBeforeA = aBounds.frontX <= bBounds.backX ||
		aBounds.frontY <= bBounds.backY ||
		aBounds.top <= bBounds.bottom;

	return (!bBeforeA && (aBeforeB || a.depth > b.depth));
}

var Bounds = function (bnds) {
	this.frontX = bnds.frontX;
	this.backX = bnds.backX;
	this.frontY = bnds.frontY;
	this.backY = bnds.backY;
	this.top = bnds.top;
	this.bottom = bnds.bottom;
};

var Data = function (data) {
	this.id = data.id;
	this.rTreeKey = data.rTreeKey;
	this.rTreeKey[4] = this.id;
	this.depth = data.depth;
	this.bounds = new Bounds(data.bounds);

	this.marked = currentFlag;
	this.after = new Map();
	this.before = new Map();
};

function depthTraversalRec1(arr) {
	currentFlag = !currentFlag;
	// !marked ==   !currentFlag

	var writeIndex = 0;
	//// recursive solution
	function rec(aData, a) {
		if (aData.marked != currentFlag) { // !marked
			aData.marked = currentFlag; // marked
			aData.after.forEach(rec);
			arr[writeIndex++] = a;
		}
	}

	map.forEach(rec);
}

function remove(id) {
	var trData = map.get(id);
	if (trData) {
		rTree.remove(trData.rTreeKey);

		trData.before.forEach(function (data, key) {
			data.after.delete(id);
		});
		trData.after.forEach(function (data) {
			data.before.delete(id);
		});
		map.delete(id);
	}
}


function add(id, aData) {
	var intrs = rTree.search(aData.rTreeKey);
	for (var j = 0; j < intrs.length; j++) {
		var b = intrs[j][4];
		var bData = map.get(b);

		if (isBefore(aData, bData)) {
			aData.after.set(b, bData);
			bData.before.set(id, aData);
		} else if (isBefore(bData, aData)) {
			aData.before.set(b, bData);
			bData.after.set(id, aData);
		}
	}

	rTree.insert(aData.rTreeKey);
	map.set(id, aData);
}

var topologicalSort = function () {
	sortScheduled = false;

	if (updated.size == 0 && removed.length == 0) {
		return;
	}

	var i;

	for (i = 0; i < removed.length; i++) {
		updated.delete(removed[i]);
		remove(removed[i]);
	}

	updated.forEach(function (newData, id) {
		remove(id);
		var data = new Data(newData);
		add(id, data);
	});

	var buffer = null;
	if (buffersPool) {
		if (buffersPool.byteLength / 4 >= map.size) {
			buffer = buffersPool;
			buffersPool = null;
		}
	}
	if (!buffer) {
		buffer = new ArrayBuffer(Math.ceil(map.size * 1.5) * 4);
	}

	var arr = new Int32Array(buffer);
	depthTraversalRec1(arr);

	updated.clear();
	removed.length = 0;

	postMessage({
		length: map.size,
		buffer: buffer
	}, [buffer]);
};

onmessage = function (msg) {
	var data = msg.data;
	switch (data.t) {
		case 'ret':
			if (!buffersPool ||
				buffersPool.byteLength < data.buffer.byteLength) {
				buffersPool = data.buffer;
			}
			break;

		case 'upd':
			data.updated.forEach(function (upd) {
				updated.set(upd.id, upd);
			});

			data.removed.forEach(function (rm) {
				removed.push(rm);
			});

			if (!sortScheduled) {
				sortScheduled = true;
				setTimeout(topologicalSort, 0);
			}
			break;
	}
};