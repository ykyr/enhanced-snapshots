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

        $scope.statusColorClass = {
          "in-use": "success",
          "creating": "error",
          "available": "info",
          "deleting": "error",
          "deleted": "error",
          "error": "error"
        };

        Regions.get().then(function (regions) {
            $scope.regions = regions
        });

        $scope.selectedRegion = $scope.globalRegion;

        // ---------filtering------------
        $scope.isCollapsed = true;
        $scope.tags = [];
        var getTags = function () {
            return $scope.tags
        };
        $scope.allTags = function (query) {
            return Tags.loadTags();
        };

        $scope.filters = {
            'volumeName': $scope.volumeName,
            'higher': $scope.higher,
            'lower': $scope.lower,
            'instanceId': $scope.instanceId,
            'tags': []
        };

        $scope.filter = {
            "name": "",
            "size": {
                "lower": 0,
                "higher": 0
            },
            "instance": "",
            "tags": []

        };

        $scope.sizeValue = "0;5";
        $scope.options = {
            from: 0,
            to: 5,
            step: 1,
            realtime: true,
            dimension: " GB",
            scale: ['|','|','|'],
            callback: function (value, released) {
                $scope.higher = parseInt( (value.split(";"))[1]);
                $scope.lower = parseInt( (value.split(";"))[0]);

            }
        };

        var volumeSizes = function (data) {
            for (var i = 0; i < data.length; i++) {
                if (data[i].size > $scope.options.to) {
                    $scope.options.to = data[i].size;
                }
            }
        };
        //----------filtering-end-----------

        $scope.isLoading = true;
        $scope.volumes = [];
        Volumes.get().then(function (data) {
            $scope.volumes = data;
            volumeSizes(data);
            $scope.isLoading = false;
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
                volumeSizes(data);
                $scope.isLoading = false;
            });
        };

        $scope.backup = function (volumeId) {
            $scope.backupVolumeId = volumeId;
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
                    backupFileName: "",
                    type: "backup",
                    status: "waiting",
                    schedulerManual: true,
                    schedulerName: Storage.get('currentUser').email,
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

        $scope.restore = function (volumeId) {
            $scope.backupToRestore = {
                fileName: '',
                volumeId: volumeId
            };
            var confirmInstance = $modal.open({
                animation: true,
                templateUrl: './partials/modal.backup-restore.html',
                scope: $scope
            });

            confirmInstance.result.then(function () {
                var newTask = {
                    id: "",
                    priority: "",
                    volume: $scope.backupToRestore.volumeId,
                    type: "restore",
                    status: "waiting",
                    schedulerManual: true,
                    schedulerName: Storage.get('currentUser').email,
                    schedulerTime: $filter('date')(new Date(), "yyyy-MM-dd HH:mm:ss") // TODO: Move time format to global setting
                };
                Tasks.insert(newTask).then(function () {
                    var successInstance = $modal.open({
                        animation: true,
                        templateUrl: './partials/modal.task-restore-created.html',
                        scope: $scope
                    });

                    successInstance.result.then(function () {
                        $state.go('app.tasks');
                    });
                });
            });

        };
    });