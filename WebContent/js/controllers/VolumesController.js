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

        $scope.tags = {};
        $scope.instances = [];
        Regions.get().then(function (regions) {
            $scope.regions = regions
        });

        $scope.selectedRegion = $scope.globalRegion;

        // ---------filtering------------

        $scope.isFilterCollapsed = true;

        $scope.sliderOptions = {
            from: 0,
            to: 16384,
            step: 4,
            dimension: " GB",
            skin: "plastic"
        };

        var processVolumes = function (data) {
            $scope.tags = {};
            $scope.instances = [""];
            for (var i = 0; i < data.length; i++){
                for (var j = 0; j < data[i].tags.length; j++){
                    var tag = data[i].tags[j];
                    if (!$scope.tags.hasOwnProperty(tag.key)){
                        $scope.tags[tag.key] = [tag.value];
                    } else {
                        if ($scope.tags[tag.key].indexOf(tag.value) == -1){
                            $scope.tags[tag.key].push(tag.value);
                        }
                    }
                }

                var instance = data[i].instanceID;
                if (instance && $scope.instances.indexOf(instance) == -1){
                    $scope.instances.push(instance);
                }
            }
            return data;
        };

        $scope.filter = {
            name: "",
            size: "0;16384",
            instanceID: "",
            region: $scope.globalRegion,
            tags: []
        };

        $scope.applyFilter = function () {
            var f = angular.copy($scope.filter);
            $scope.stAdvancedFilter = {
                "volumeName": {
                    "type": "str",
                    "value": f.name
                },
                "size": {
                    "type": "int-range",
                    "value": {
                        "lower": parseInt(f.size.split(";")[0], 10),
                        "higher": parseInt(f.size.split(";")[1], 10)
                    }
                },
                "instanceID": {
                    "type": "str-strict",
                    "value": f.instanceID
                },
                "availabilityZone": {
                    "type": "str",
                    "value": f.region.id
                },
                "tags": {
                    "type": "array-inc",
                    "value": f.tags
                }
            };
        };

        //----------filtering-end-----------

        $scope.isLoading = true;
        $scope.volumes = [];


        Volumes.get().then(function (data) {
            $scope.volumes = processVolumes(data);
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
                getTags();
                $scope.isLoading = false;
            }, function () {
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