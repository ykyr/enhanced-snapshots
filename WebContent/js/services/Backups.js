'use strict';

angular.module('web')
    .service('Backups', function ($q, $http, Storage, BASE_URL) {
        var url = BASE_URL + 'rest/backup';
        var storageKey = '_backups';

        var getAll = function () {
            var deferred = $q.defer();
            if (Storage.get(storageKey)) {
                deferred.resolve(Storage.get(storageKey));
            } else {
                $http.get(url).success(function (data) {
                    Storage.save(storageKey, data);
                    deferred.resolve(data);
                });
            }
            return deferred.promise;
        };

        var save = function (item) {
            return getAll().then(function (data) {
                for (var i = 0; i < data.length; i++) {
                    if (data[i].id == item.id){
                        data[i] = item;
                        Storage.save(storageKey, data);
                        return true;
                    };
                }

            });
        };

        var remove = function (id) {
            return getAll().then(function (data) {
                var newData = data.filter(function (i) {
                    return i.id != id;
                });
                Storage.save(storageKey, newData);
                return true;
            })
        };

        var add = function (item) {
            return getAll().then(function (data) {
                var maxId = Math.max.apply(Math,data.map(function(i){return i.id;}));
                item.id = maxId + 1;
                data.push(item);
                Storage.save(storageKey, data);
                return true;
            });
        };

        return {
            getForVolume: function (volume) {
                return getAll().then(function(data){
                    return data.filter(function (s) {
                        return s.volume == volume;
                    });
                });
            },
            update: function (item) {
                return save(item);
            },
            insert: function (item) {
                return add(item);
            },
            delete: function (id){
                return remove(id);
            }
        }
    });