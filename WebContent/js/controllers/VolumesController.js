'use strict';

angular.module('web')
    .controller('VolumesController', function ($scope, $state, $filter, ITEMS_BY_PAGE, DISPLAY_PAGES, $modal, Volumes, Tasks) {

        $scope.itemsByPage = ITEMS_BY_PAGE;
        $scope.displayedPages = DISPLAY_PAGES;

        $scope.globalRegion = {
            location: "",
            name: "Global",
            id: ""
        };
        $scope.regions = [
            {
                location: "Virginia",
                name: "US East 1",
                id: "us-east-1"
            },
            {
                location: "California",
                name: "US West 1",
                id: "us-west-1"
            },
            {
                location: "Oregon",
                name: "US West 2",
                id: "us-west-2"
            },
            {
                location: "Ireland",
                name: "EU West 1",
                id: "eu-west-1"
            },
            {
                location: "Singapore",
                name: "AP Southeast 1",
                id: "ap-southeast-1"
            },
            {
                location: "Tokyo",
                name: "AP Northeast 1",
                id: "ap-northeast-1"
            },
            {
                location: "Sydney",
                name: "AP Southeast 2",
                id: "ap-southeast-2"
            },
            {
                location: "São Paulo",
                name: "SA East 1",
                id: "sa-east-1"
            }
        ];
        $scope.selectedRegion = $scope.globalRegion;

        $scope.volumes = [];
        Volumes.get().then(function (data) {
            $scope.volumes = data;
            $scope.displayedVolumes = data;
        });

        $scope.changeRegion = function (region) {
            $scope.selectedRegion = region;
        };

        $scope.refresh = function () {
            Volumes.refresh().then(function (data) {
                $scope.volumes = data;
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
                    volume: $scope.backupVolumeId,
                    type: "backup",
                    status: "waiting",
                    scheduler: {
                        manual: true,
                        name: "admin", // TODO: Real user name should be used here
                        time: $filter('date')(new Date(), "yyyy-MM-dd HH:mm:ss") // TODO: Move time format to global setting
                    }
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