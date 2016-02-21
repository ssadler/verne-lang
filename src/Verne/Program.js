/* global exports */
"use strict";

// module Verne.Program

exports.make = function(ps) {

    var m = function(val) {
        return ps.maybe(null)(function(v) {return v})(val);
    }
    var e = function(val) {
        return ps.either(function(v) {return  {left:v}})(function(v) {return {right:v}})(val);
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
            return e(this.run(ps.parse(str)));
        },
        compile: function(code) {
            return e(this.run(ps.compile(code)));
        },
        addComponent: function(component) {
            return ps.addComponent(component);
        },
        getCompletion: function(caret, code) {
            return m(this.run(ps.getCompletion(caret)(code)));
        }
    };
    return Program;

};
