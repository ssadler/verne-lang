/* global exports */
"use strict";

// module Verne.Exec

var Left = PS['Data.Either'].Left;
var Right = PS['Data.Either'].Right;

exports.runPart = function(f, args) {
    try {
        return Right(f.execute.apply(f, args));
    } catch (e) {
        return Left(e);
    }
};
