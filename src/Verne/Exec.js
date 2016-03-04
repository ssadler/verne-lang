/* global exports */
"use strict";

// module Verne.Exec

var Left = PS['Data.Either'].Left;
var Right = PS['Data.Either'].Right;

var runPartInner = function(part, moreargs) {
    var args = part.args.map(runPartInner).concat(moreargs || []);
    return part.exec.apply(part, args);
}

exports.runPart = function(part, moreargs) {
    try {
        return new Right(runPartInner(part, moreargs));
    } catch (e) {
        return new Left(e);
    }
};
