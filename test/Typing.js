/* global exports */
"use strict";

// module Test.Typing

var sayHi = {value0: {signature: ["IO ()"]}};

var setBackgroundColor = {value0: {signature: ["IO ()", "String"]}};

exports.registry = {
    componentByName: { "sayHi": sayHi
                     , "setBackgroundColor": setBackgroundColor
                     },
    componentsByTypeHead: { "IO ()": [sayHi, setBackgroundColor]}
};

