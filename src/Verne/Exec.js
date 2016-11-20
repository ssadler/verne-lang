/* global exports */
"use strict";

// module Verne.Exec

var runPartInner = function(part, moreargs) {
    var args = part.args.map(runPartInner).concat(moreargs || []);
    return part.exec.apply(part, args);
}

var either = require('Data.Either');

exports.runPart = function(left, right, part, moreargs) {
    try {
        return new either.Right(runPartInner(part, moreargs));
    } catch (e) {
        return new either.Left(e);
    }
};
