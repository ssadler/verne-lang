/* global exports */
"use strict";

// module Test.Main

exports.titlePart = {
    name: "title",
    type: "String -> Effect",
    exec: function(title) {
        document.title = title;
    }
};

exports.reloadPart = {
    name: 'reload',
    type: "Effect",
    exec: function() {
        location.reload();
        verne.shell.term.echo('reloading...');
    }
};

exports.autocompletePart = {
    name: 'autoc',
    type: 'Effect',
    exec: function() {},
    autocomplete: function() {}
};

exports.dump = function(v) { return function() { console.log(v); } }
