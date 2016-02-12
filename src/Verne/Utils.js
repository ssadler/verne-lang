/* global exports */
"use strict";

// module Verne.Utils

exports.dump = function(v) { return v + "" };

exports.compactShow = function(s) { return s.replace(/[A-Z][a-zA-Z0-9_]+\./g, ''); }

exports.isSame = function(a,b) { return a === b }

var cryptoJs = require('crypto-js');

exports.hashOne = function(s) { return exports.hashParts([s]) };

exports.nullValue = function(_) { return null; }

/*
 * Hash an array in a non collidable way
 */
exports.hashMany = function(args) {
    var s = cryptoJs.SHA256();
    s.extend(arguments.length.toString());
    args.forEach(function(part) { s.extend(part) });
    args.forEach(function(part) {
        s.extend('|');
        s.extend(part.length);
    });
    return s.toString();
}
