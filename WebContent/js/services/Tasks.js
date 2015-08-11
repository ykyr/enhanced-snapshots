'use strict';

angular.module('web')
    .service('Tasks', function ($q, $http, Storage, BASE_URL) {
        var url = BASE_URL + 'rest/task';
        var storageKey = '_tasks';

        var getAll = function () {
            var deferred = $q.defer();
            $http.get(url).success(function (data) {
                deferred.resolve(data);
            });
            return deferred.promise;
        };

        var save = function (item) {
            return getAll().then(function (data) {
                for (var i = 0; i < data.length; i++) {
                    if (data[i].id == item.id) {
                        data[i] = item;
                        Storage.save(storageKey, data);
                        return true;
                    }
                }
            });
        };

        var remove = function (id) {
            return $http({
                url: url + "/" + id,
                method: "DELETE"
            })
        };

        var add = function (item) {
            return $http({
                url: url,
                method: "POST",
                data: item
            });
        };

        return {
            get: function () {
                return getAll();
            },
            update: function (item) {
                return save(item);
            },
            insert: function (item) {
                return add(item);
            },
            delete: function (id) {
                return remove(id);
            }
        }
    });