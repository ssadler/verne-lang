/* global exports */
"use strict";

// module Verne.Api

var Either = require('Data.Either');
var DataCode = require('Verne.Data.Code');

exports.make = function(ps) {
    var m = function(val) { return val.value0; }
    var e = function(val) {
        return val instanceof either.Left ? 
            {left: val.value0} : {right: val.value0};
    };
    var ex = function(val) {
        if (val instanceof either.Left) {
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
            var code = m(ps.getCodeAtPosition(caret)(this.code));
            if (!code) return;
            var completion = {
                a: code.value0,
                b: code.value1
            };
            code = code.value2;
            console.log(code.constructor.name);
            if (code instanceof DataCode.Undefined) {
                console.log('its undefined');
                var gnc = ps.getNameCompletions(code.value0)(code.value1)
                completion.names = this.program.run(gnc);
            } else if (code instanceof DataCode.Atom) {
                completion.part = code.value0;
            } else if (code instanceof DataCode.NeedsArgument) {
                var gnc = ps.getNameCompletions("")(code.value0)
                completion.names = this.program.run(gnc);
                console.log(names);
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
            return ex(this.run(ps.importPart(object)));
        },
        compile: function(str) {
            return new Code(this, str);
        },
    };
    return Program;
};
