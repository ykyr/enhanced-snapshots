'use strict';

angular.module('web')
    .service('Users', function ($q, $http, Storage, BASE_URL) {
        var url = BASE_URL + "rest/user";
        var storageKey = '_users';

        var getUsers = function () {
            var deferred = $q.defer();
            $http({
                url: url,
                method: 'GET'
            }).success(function (data) {
                deferred.resolve(data);
            });
            return deferred.promise;
        };

        var add = function (user) {
            var deferred = $q.defer();
            $http({
                url: url,
                method: 'POST',
                data: user
            }).success(function (data) {
                deferred.resolve(data);
            });
            return deferred.promise;
        };

        var updateUser = function (user) {
            return $http({
                url: url,
                method: 'PUT',
                data: user
            })
        };

        var getCurrentUser = function () {
            return Storage.get('currentUser')
        };

        var remove = function (email) {
            return $http({
                url: url + "/" + email,
                method: 'DELETE'
            })
        };

        return {
            insert: function (user) {
                return add(user);
            },

            delete: function (email) {
                return remove(email);
            },

            update: function (user) {
                return updateUser(user);
            },

            getCurrent: function () {
                return getCurrentUser();
            },
            getAll: function () {
                return getUsers().then(function (data) {
                    return data;
                })
            }
        }
    });