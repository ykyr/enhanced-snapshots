'use strict';

angular.module('web')
    .service('Configuration', function ($q, $http, BASE_URL) {
        var url = BASE_URL + 'rest/configuration';

        var _get = function (type) {
            var deferred = $q.defer();
            $http({
                url: url + "/" + type,
                method: 'GET'
            }).then(function (data, status) {
                deferred.resolve(data, status);
            }, function (data, status) {
                deferred.reject(data, status)
            });
            return deferred.promise;
        };


        var _send = function (type, item, timeout) {
            var deferred = $q.defer();
            var request = {
                url: url + "/" + type,
                method: "POST",
                data: item || {}
            };
            if (timeout) {request.timeout = timeout;}

            $http(request).then(function () {
                deferred.resolve()
            }, function (data, status) {
                deferred.reject(data, status)
            });
            return deferred.promise;
        };

        return {
            get: function (type) {
                return _get(type);
            },
            send: function (type, item, timeout) {
                return _send(type, item, timeout);
            }
        }
    });