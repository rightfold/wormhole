'use strict';

exports.slugify = function(s) {
    return s.toLowerCase().replace(/[^a-z0-9]+/g, '-');
};
