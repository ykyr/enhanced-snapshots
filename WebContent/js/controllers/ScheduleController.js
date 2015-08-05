'use strict';

angular.module('web')
    .controller('ScheduleController', function ($scope, $stateParams, $filter, Schedules, $modal) {

        $scope.volumeID = $stateParams.volumeID;
        $scope.schedules = [];


        var showModal = function (schedule) {

            var modalInstance = $modal.open({
                animation: true,
                templateUrl: './partials/modal.schedule-edit.html',
                controller: 'modalScheduleCtrl',
                scope: $scope,
                resolve: {
                    schedule: function () {
                        return schedule;
                    }
                }
            });

            modalInstance.result.then(function () {
                refreshList();
            });
        };

        $scope.displayEnd = function (end) {
            if (typeof end != 'undefined' && end) {
                return $filter('date')(end, "yyyy-MM-dd HH:mm:ss");
            }
            else {
                return "Never";
            }
        };

        var refreshList = function () {
            Schedules.getForVolume($scope.volumeID).then(function (data) {
                $scope.schedules = data;
            });
        };

        refreshList();

        $scope.add = function () {
            var newSchedule =
            {
                id: 0,
                volumeID: $scope.volumeID,
                name: "",
                start: new Date(),
                interval: {
                    amount: "1",
                    unit: "day"
                },
                week: []
            };
            showModal(newSchedule);
        };

        $scope.edit = function (schedule) {
            schedule.recurrent = schedule.recurrent != "false";
            schedule.start = new Date(schedule.start);
            schedule.end = (function () {
                if (schedule.end != 'undefined' && schedule.end) {
                    return new Date(schedule.end);
                }
                else return "";
            })();

            showModal(schedule);
        };

        $scope.remove = function (schedule) {
            $scope.schedule = schedule;
            var confirmInstance = $modal.open({
                animation: true,
                templateUrl: './partials/modal.schedule-del.html',
                scope: $scope
            });


            confirmInstance.result.then(function () {
                Schedules.delete(schedule.id).then(function (data) {
                    refreshList();
                });
            });
        };
    });