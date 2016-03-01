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
    var Program = function() {
        this.state = ps.newProgramState;
    };
    Program.prototype = {
        run: function(act) {
            var tup = PS['Control.Monad.State'].runState(act)(this.state);
            this.state = tup.value1;
            return tup.value0;
        },
        parse: function(str) {
            return e(this.run(ps.parse(str)));
        },
        compileString: function(str) {
            var syntax = e(this.run(ps.parse(str)));
            if (syntax.right) {
                return e(this.run(ps.compile(syntax.right)));
            }
            return syntax;
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
