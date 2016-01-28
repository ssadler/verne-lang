/* global exports */
"use strict";

// module Language.Verne.Utils

exports.compactShow = function(s) { return s.replace(/[A-Z][a-zA-Z0-9_]+\./g, ''); }

