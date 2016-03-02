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
            throw val.value0;
        } else {
            return val.value0;
        }
    };

    var Code = function(program, str) {
        this.program = program;
        this.syntax = ex(program.run(ps.parse(str)));
        this.code = program.run(ps.compile(this.syntax));
    }
    Code.prototype = {
        execute: function() {
            var exe = ex(ps.toExecutable(this.code));
            return ps.execute(exe);
        },
        getCompletion: function(caret, code) {
            return m(this.run(ps.getCompletion(caret)(code)));
        }
    }
    
    var Program = function() {
        this.state = ps.newProgramState;
    };
    Program.prototype = {
        run: function(act) {
            var tup = ps.runState(act)(this.state);
            this.state = tup.value1;
            return tup.value0;
        },
        addPart: function(object) {
            return ex(this.run(ps.addPart(object)));
        },
        compile: function(str) {
            return new Code(this, str);
        },
    };
    return Program;
};
