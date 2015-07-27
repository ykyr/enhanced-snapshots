/**
 * Created by Administrator on 24.07.2015.
 */
'use strict';

angular.module('web')
    .service('Users', function ($q, $http, Storage, BASE_URL) {
        // var url = BASE_URL + "rest/user";
        var storageKey = '_users';

        var getAll = function () {
            var deferred = $q.defer();
            if (Storage.get(storageKey)) {
                deferred.resolve(Storage.get(storageKey));
            } else {
                deferred.resolve([]);
            }
            return deferred.promise;
        };

        var add = function (user) {
            return getAll().then(function (data) {
                data.push(user);
                Storage.save(storageKey, data);
                return true;
            });
        };

        return {
            insert: function (user) {
                return add(user);
            },

            getAllUsers: function () {
                return getAll().then(function (data) {
                    return data;
                })
            }
        }
    });