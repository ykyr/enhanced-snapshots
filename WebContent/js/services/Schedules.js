'use strict';

angular.module('web')
    .service('Schedules', function ($q, $http, Storage, BASE_URL) {
        var url = BASE_URL + 'rest/schedule';
        var storageKey = '_schedules';

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

        var save = function (schedule) {
            return getAll().then(function (data) {
                for (var i = 0; i < data.length; i++) {
                    if (data[i].id == schedule.id){
                        data[i] = schedule;
                        Storage.save(storageKey, data);
                        return true;
                    };
                }

            });
        };

        var remove = function (id) {
            return getAll().then(function (data) {
                var newData = data.filter(function (s) {
                    return s.id != id;
                });
                Storage.save(storageKey, newData);
                return true;
            })
        };

        var add = function (schedule) {
            return getAll().then(function (data) {
                var maxId = Math.max.apply(Math,data.map(function(s){return s.id;}));
                schedule.id = maxId + 1;
                data.push(schedule);
                Storage.save(storageKey, data);
                return true;
            });
        };

        return {
            getForVolume: function (volumeID) {
                return getAll().then(function(data){
                    return data.filter(function (s) {
                        return s.volumeID == volumeID;
                    });
                });
            },
            update: function (schedule) {
                return save(schedule);
            },
            insert: function (schedule) {
                return add(schedule);
            },
            delete: function (id){
                return remove(id);
            }
        }
    });