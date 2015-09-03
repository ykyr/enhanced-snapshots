'use strict';

angular.module('web')
    .service('Volumes', function ($q, $http, Storage, BASE_URL) {
        var url = BASE_URL + 'rest/volume';
        var storageKey = 'volumes';

        return {
            get: function () {
                var deferred = $q.defer();
                if (Storage.get(storageKey)) {
                    deferred.resolve(Storage.get(storageKey));
                } else {
                    $http.get(url).then(function (data) {
                        Storage.save(storageKey, data);
                        deferred.resolve(data);
                    }, function (data, status) {
                        deferred.reject(data, status)
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
                }).error(function(msg){
                    // TODO: handle 401 here
                });

                return deferred.promise;
            }
        }
    });