'use strict';

angular.module('web')
    .service('Volumes', function ($q, $http, Storage) {
        var url = './rest/volume';
        var storageKey = 'volumes';

        return {
            get: function () {
                var deferred = $q.defer();
                if (Storage.get(storageKey) && false) {
                    deferred.resolve(Storage.get(storageKey));
                } else {
                    $http.get(url).success(function (data) {
                        Storage.save(storageKey, data);
                        deferred.resolve(data);
                    });
                }
                return deferred.promise;
            },
            refresh: function() {
                var deferred = $q.defer();

                Storage.remove(storageKey);
                $http.get(url).success(function (data) {
                    Storage.save(storageKey, data);
                    deferred.resolve(data);
                });

                return deferred.promise;
            }
        }
    });