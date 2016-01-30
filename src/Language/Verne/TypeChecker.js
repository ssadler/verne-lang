/* global exports */
"use strict";

// module Language.Verne.TypeChecker

exports.toComponentOriginal = function(s) {
    return {
        exec: function() { return s; }
    };
}

