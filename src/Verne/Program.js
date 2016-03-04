/* global exports */
"use strict";

// module Verne.Program


exports.make = function(ps) {
    var DC = PS['Verne.Data.Code'];
    var DT = PS['Verne.Data.Type'];
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
        this.str = str;
        this.syntax = ex(ps.parse(str));
        this.code = program.run(ps.compile(this.syntax));
    }
    Code.prototype = {
        execute: function() {
            var exe = ex(ps.toExecutable(this.code));
            return ps.execute(exe);
        },
        getCompletion: function(caret) {
            var code = m(ps.getCompletion(caret)(this.code));
            if (!code) return;
            var completion = {
                a: code.value0,
                b: code.value1
            };
            code = code.value2;
            if (code instanceof DC.Undefined) {
                var gnc = ps.getNameCompletions(code.value0)(code.value1)
                completion.names = this.program.run(gnc);
            } else if (code instanceof DC.Atom) {
                completion.part = code.value0;
            } else if (code instanceof DC.NeedsArgument) {
                var gnc = ps.getNameCompletions("")(code.value0)
                completion.names = this.program.run(gnc);
            } else {
                console.log('unrecognized completion', code);
                return;
            }
            return completion;
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
