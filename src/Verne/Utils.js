/* global exports */
"use strict";

// module Verne.Utils

exports.infinity = Infinity;

exports.dump = function(v) { return v + "" };

exports.compactShow = function(s) { return s.replace(/[A-Z][a-zA-Z0-9_]+\./g, ''); }

exports.isSame = function(a,b) { return a === b }

exports.hashOne = function(s) { return exports.hashMany([s]) };

exports.nullValue = function(_) { return null; }

exports.freeze = function(v) { module.require('icepick').freeze(v); }

/*
 * Hash an array in a non collidable way
 */
exports.hashMany = function(args) {
    var cryptoJs = require('crypto-js');
    var s = cryptoJs.SHA256();
    s.extend(arguments.length.toString());
    args.forEach(function(part) { s.extend(part) });
    args.forEach(function(part) {
        s.extend('|');
        s.extend(part.length);
    });
    return s.toString();
}

exports.curryForeign = function(exec1, exec2) {
    return function() {
        var args = [exec2()];
        args.push.apply(args, arguments);
        return exec1.apply(this, args);
    };
};

exports.autoCurry = function(exec, autocomplete) {
    return function(moreArgs, callback) {
        var args = [exec()];
        args.push.apply(args, moreArgs);
        return autocomplete(args, callback);
    };
};
