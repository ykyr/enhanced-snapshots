'use strict';

angular.module('web')
    .service('System', function ($q, $http, BASE_URL) {
        var url = BASE_URL + 'rest/backup/system';

        var _get = function () {
            var deferred = $q.defer();
            $http.get(url).then(function (result) {
                deferred.resolve(result.data);
            }, function (e) {
                deferred.reject(e);
            });
            return deferred.promise;
        };

        return {
            get: function () {
                return _get();
            }
        }
    });