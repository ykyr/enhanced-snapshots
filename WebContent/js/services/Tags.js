'use strict';

angular.module('web')
    .service('Tags', function ($q) {
        var tags = [
            { "text": "Tag1" },
            { "text": "Tag2" },
            { "text": "Tag3" },
            { "text": "Tag4" },
            { "text": "Tag5" },
            { "text": "Tag6" },
            { "text": "Tag7" },
            { "text": "Tag8" },
            { "text": "Tag9" },
            { "text": "Tag10" }
        ];

        this.loadTags = function() {
            var deferred = $q.defer();
            deferred.resolve(tags);
            return deferred.promise;
        };
    });