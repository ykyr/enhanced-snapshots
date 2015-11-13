'use strict';

angular.module('web')
    .controller('VolumesController', function ($scope, $rootScope, $state, Retention, $filter, Storage, Regions, ITEMS_BY_PAGE, DISPLAY_PAGES, $modal, Volumes, Tasks) {
        $scope.maxVolumeDisplay = 5;
        $scope.itemsByPage = ITEMS_BY_PAGE;
        $scope.displayedPages = DISPLAY_PAGES;

        $scope.globalRegion = {
            location: "",
            name: "GLOBAL",
            id: ""
        };

        $scope.stateColorClass = {
            "in-use": "success",
            "creating": "error",
            "available": "info",
            "deleting": "error",
            "deleted": "error",
            "error": "error",
            "removed": "danger"

        };

        $scope.textClass = {
            'false': 'Select',
            'true': 'Unselect'
        };

        $scope.iconClass = {
            'false': 'unchecked',
            'true': 'check'
        };

        var actions = {
            backup: {
                type: 'backup',
                bgClass: 'primary',
                modalTitle: 'Backup Volume',
                iconClass: 'cloud-download',
                description: 'start backup task',
                buttonText: 'Add backup task'
            },
            restore: {
                type: 'restore',
                bgClass: 'success',
                modalTitle: 'Restore Backup',
                iconClass: 'cloud-upload',
                description: 'start restore task',
                buttonText: 'Add restore task'

            },
            schedule: {
                type: 'schedule',
                bgClass: 'warning',
                modalTitle: 'Add Schedule',
                iconClass: 'time',
                description: 'add schedule',
                buttonText: 'Add schedule'
            }
        }

        $scope.isAllSelected = false;
        $scope.selectedAmount = 0;

        $scope.checkAllSelection = function () {
            var disabledAmount = $scope.volumes.filter(function (v) { return $scope.isDisabled(v)}).length;
            $scope.selectedAmount = $scope.volumes.filter(function (v) { return v.isSelected}).length;
            $scope.isAllSelected = ($scope.selectedAmount + disabledAmount == $scope.volumes.length);
        };

        $scope.selectAll = function () {
            $scope.volumes.forEach(function (volume) {
                doSelection(volume, !$scope.isAllSelected);
            });
            $scope.checkAllSelection();
        };

        $scope.toggleSelection = function (volume) {
            doSelection(volume, !volume.isSelected);
            $scope.checkAllSelection();
        };

        var doSelection = function (volume, value) {
            if(volume.hasOwnProperty('isSelected')) {
                volume.isSelected = value;
            }
        };

        $scope.tags = {};
        $scope.instances = [];
        Regions.get().then(function (regions) {
            $scope.regions = regions
        });

        $scope.selectedRegion = $scope.globalRegion;

        $scope.isDisabled = function (volume) {
            return volume.state === 'removed'
        };

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
                if (data[i].state !== 'removed') data[i].isSelected = false;
            }
            $scope.isAllSelected = false;
            return data;
        };

        $scope.filter = {
            volumeId: "",
            name: "",
            size: "0;16384",
            instanceID: "",
            region: $scope.globalRegion,
            tags: []
        };

        $scope.applyFilter = function () {
            var f = angular.copy($scope.filter);
            $scope.stAdvancedFilter = {
                "volumeId": {
                    "type": "str",
                    "value": f.volumeId
                },
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

        //-----------Volumes-get/refresh-------------

        $scope.changeRegion = function (region) {
            $scope.selectedRegion = region;
        };

        $scope.refresh = function () {
            $rootScope.isLoading = true;
            $scope.volumes = [];
            Volumes.get().then(function (data) {
                $scope.volumes = processVolumes(data);
                $rootScope.isLoading = false;
            }, function () {
                $rootScope.isLoading = false;
            });
        };

        $scope.refresh();
        //-----------Volumes-get/refresh-end------------

        //-----------Volume-backup/restore/retention-------------

        $scope.volumeAction = function (actionType) {
            $scope.selectedVolumes = $scope.volumes.filter(function (v) { return v.isSelected; });
            $scope.actionType = actionType;
            $scope.action = actions[actionType];
            $scope.schedule = { name: '', cron: '', enabled: true };

            var confirmInstance = $modal.open({
                animation: true,
                templateUrl: './partials/modal.volumeAction.html',
                scope: $scope
            });

            confirmInstance.result.then(function () {
                $rootScope.isLoading = true;
                var volList = $scope.selectedVolumes.map(function (v) { return v.volumeId; });

                var getNewTask = function(){
                    var newTask = {
                        id: "",
                        priority: "",
                        volumes: volList,
                        status: "waiting"
                    };

                    switch (actionType) {
                        case 'backup':
                        case 'restore':
                            newTask.type = actionType;
                            newTask.schedulerManual = true;
                            newTask.schedulerName = Storage.get('currentUser').email;
                            newTask.schedulerTime = Date.now();
                            break;
                        case 'schedule':
                            newTask.type = 'backup';
                            newTask.regular = true;
                            newTask.schedulerManual = false;
                            newTask.schedulerName = $scope.schedule.name;
                            newTask.cron = $scope.schedule.cron;
                            newTask.enabled = $scope.schedule.enabled;
                            break;
                    }

                    return newTask;
                };

                var t = getNewTask();
                Tasks.insert(t).then(function () {
                    $rootScope.isLoading = false;
                    if (actionType != 'schedule') {
                        var successInstance = $modal.open({
                            animation: true,
                            templateUrl: './partials/modal.task-created.html',
                            scope: $scope
                        });

                        successInstance.result.then(function () {
                            $state.go('app.tasks');
                        });
                    }
                }, function (e) {
                    $rootScope.isLoading = false;
                    console.log(e);
                });

            });

        };

        var getShowRule = function (rule) {
            var showRules = {};
            angular.forEach($scope.rule, function (value, key) {
                showRules[key] = value > 0;
            });
            Object.defineProperty(showRules, 'never', {
                get: function() {
                    return !$scope.showRetentionRule.size && !$scope.showRetentionRule.count && !$scope.showRetentionRule.days;
                },
                set: function(value) {
                    if (value){
                        $scope.showRetentionRule.size = false;
                        $scope.showRetentionRule.count = false;
                        $scope.showRetentionRule.days = false;
                    }
                }
            });
            return showRules;
        };
        $scope.retentionRule = function (volume) {
            $rootScope.isLoading = true;
            Retention.get(volume.volumeId).then(function (data) {

                $scope.rule = {
                    size: data.size,
                    count: data.count,
                    days: data.days
                };
                $scope.showRetentionRule = getShowRule($scope.rule);

                $rootScope.isLoading = false;

                var retentionModalInstance = $modal.open({
                    animation: true,
                    templateUrl: './partials/modal.retention-edit.html',
                    scope: $scope
                });

                retentionModalInstance.result.then(function () {
                    $rootScope.isLoading = true;
                    var rule = angular.copy($scope.rule);
                    angular.forEach(rule, function (value, key) {
                        rule[key] = $scope.showRetentionRule[key] ? rule[key] : 0
                    });
                    rule.volumeId = data.volumeId;

                    Retention.update(rule).then(function () {
                        $rootScope.isLoading = false;
                    }, function () {
                        $rootScope.isLoading = false;
                    })
                });

            }, function () {
                $rootScope.isLoading = false;
            });

        }
    });