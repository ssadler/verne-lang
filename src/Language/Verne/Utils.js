/* global exports */
"use strict";

// module Language.Verne.Utils

exports.dump = v => v + "";

exports.compactShow = function(s) { return s.replace(/[A-Z][a-zA-Z0-9_]+\./g, ''); }

exports.isSame = function(a,b) { return a === b }

var cryptoJs = require('crypto-js');

exports.hashOne = s => exports.hashParts([s]);

/*
 * Hash an array in a non collidable way
 */
exports.hashMany = args => {
    var s = cryptoJs.SHA256();
    s.extend(arguments.length.toString());
    args.forEach(part => s.extend(part));
    args.forEach(part => {
        s.extend('|');
        s.extend(part.length);
    });
    return s.toString();
}
