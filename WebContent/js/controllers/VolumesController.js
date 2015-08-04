'use strict';

angular.module('web')
    .controller('VolumesController', function ($scope, $state, $filter, Storage, Regions, ITEMS_BY_PAGE, DISPLAY_PAGES, $modal, Volumes, Tasks) {

        $scope.itemsByPage = ITEMS_BY_PAGE;
        $scope.displayedPages = DISPLAY_PAGES;

        $scope.globalRegion = {
            location: "",
            name: "GLOBAL",
            id: ""
        };

        Regions.get().then(function (regions) {
            $scope.regions = regions
        });

        $scope.selectedRegion = $scope.globalRegion;

        $scope.isLoading = true;
        $scope.volumes = [];
        Volumes.get().then(function (data) {
            $scope.isLoading = false;
            $scope.volumes = data;
        }, function () {
            $scope.isLoading = false;
        });

        $scope.changeRegion = function (region) {
            $scope.selectedRegion = region;
        };

        $scope.refresh = function () {
            $scope.isLoading = true;
            $scope.volumes = undefined;
            Volumes.refresh().then(function (data) {
                $scope.volumes = data;
                $scope.isLoading = false;
            });
        };

        $scope.backup = function (volumeID) {
            $scope.backupVolumeId = volumeID;
            var confirmInstance = $modal.open({
                animation: true,
                templateUrl: './partials/modal.backup-now.html',
                scope: $scope
            });

            confirmInstance.result.then(function () {
                var newTask = {
                    id: "",
                    priority: "",
                    volume: $scope.backupVolumeId,
                    type: "backup",
                    status: "waiting",
                    schedulerManual: true,
                    schedulerName: Storage.get('currentUser').username, // TODO: Real user name should be used here
                    schedulerTime: $filter('date')(new Date(), "yyyy-MM-dd HH:mm:ss") // TODO: Move time format to global setting
                };
                Tasks.insert(newTask).then(function () {
                    var successInstance = $modal.open({
                        animation: true,
                        templateUrl: './partials/modal.task-backup-created.html'
                    });

                    successInstance.result.then(function () {
                        $state.go('app.tasks');
                    });
                });
            });

        };
    });