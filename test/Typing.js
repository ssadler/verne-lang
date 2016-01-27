/* global exports */
"use strict";

// module Test.Typing

var sayHi = {value0: {signature: ["IO ()"]}};

exports.registry = {
    componentByName: {"sayHi": sayHi},
    componentsByTypeHead: {"IO ()": [sayHi]}
};

