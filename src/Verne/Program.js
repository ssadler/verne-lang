/* global exports */
"use strict";

// module Verne.Program

exports.make = function(ps) {
    var m = function(val) { return val.value0; }
    var e = function(val) {
        return val instanceof PS['Data.Either'].Left ? 
            {left: val.value0} : {right: val.value0};
    };
    var ex = function(val) {
        if (val instanceof PS['Data.Either'].Left) {
            throw val;
        } else {
            return val;
        }
    };
    var Program = function() {
        this.state = ps.newProgramState;
    };
    Program.prototype = {
        run: function(act) {
            var tup = ps.runState(act)(this.state);
            this.state = tup.value1;
            return tup.value0;
        },
        parse: function(str) {
            return ex(this.run(ps.parse(str)));
        },
        compileString: function(caret, str) {
            var code = this.parse(str);
            var r = this.run(ps.compile(caret)(code));
            if (r instanceof PS['Verne.Program.Compiler.Coroutine'].Run) {
                return {run: r.value0};
            } else {
                return { cont: r.value0 , yield: e(r.value1) };
            };
        },
        addPart: function(object) {
            return ex(this.run(ps.addPart(object)));
        },
        getCompletion: function(caret, code) {
            return m(this.run(ps.getCompletion(caret)(code)));
        }
    };
    return Program;
};
