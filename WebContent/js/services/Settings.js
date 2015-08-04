'use strict';

angular.module('web')
    .service('Settings', function ($q, $http, Storage) {
        var storageKey = '_settings';

        return {
            get: function () {
                return Storage.get(storageKey);
            },
            save: function (data) {
                Storage.save(storageKey, data);
                return true;
            }
        }
    });