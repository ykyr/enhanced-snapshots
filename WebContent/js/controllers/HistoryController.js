'use strict';

angular.module('web')
    .controller('HistoryController', function ($scope, Storage, ITEMS_BY_PAGE, DISPLAY_PAGES, $stateParams, $state, $modal, $filter, Backups, Tasks) {
        $scope.maxDeleteBackupDisplay = 5;

        $scope.itemsByPage = ITEMS_BY_PAGE;
        $scope.displayedPages = DISPLAY_PAGES;

        $scope.volumeId = $stateParams.volumeId;

        $scope.textClass = {
            'false': 'select',
            'true': 'unselect'
        };

        $scope.iconClass = {
            'false': 'unchecked',
            'true': 'check'
        };

        $scope.isAllSelected = false;
        $scope.selectedAmount = 0;

        $scope.checkSelection = function () {
            $scope.selectedAmount = $scope.backups.filter(function (b) { return b.isSelected; }).length;
            $scope.isAllSelected = $scope.selectedAmount == $scope.backups.length;
        };

        $scope.makeSelection = function () {
            $scope.backups.forEach(function (backup) {
                backup.isSelected = !$scope.isAllSelected;
            });
            $scope.checkSelection();
        };

        $scope.deleteSelection = function () {
            $scope.selectedBackups = $scope.backups.filter(function (b) { return b.isSelected; });

            var confirmInstance = $modal.open({
                animation: true,
                templateUrl: './partials/modal.backup-delete.html',
                scope: $scope
            });

            confirmInstance.result.then(function () {
                $scope.isLoading = true;
                $scope.deleteErrors = [];

                var fileNames = $scope.selectedBackups.map(function (b) { return b.fileName });
                var remaining = fileNames.length;

                var checkDeleteFinished = function () {
                    $scope.isLoading = remaining > 0;
                    if (!$scope.isLoading){
                        if ($scope.deleteErrors.length) { console.log($scope.deleteErrors); }
                        var finishedInstance = $modal.open({
                            animation: true,
                            templateUrl: './partials/modal.backup-delete-result.html',
                            scope: $scope
                        });

                        finishedInstance.result.then(function () {
                            $state.go('app.tasks');
                        }, function () {
                            loadBackups();
                        });
                    }
                };

                for (var i = 0; i < fileNames.length; i++) {
                    Backups.delete(fileNames[i]).then(function () {
                        remaining--;
                        checkDeleteFinished();
                    }, function (e) {
                        $scope.deleteErrors.push(e);
                        remaining--;
                        checkDeleteFinished();
                    })
                }
            })
        };

        $scope.isLoading = false;
        $scope.backups = [];
        var loadBackups = function () {
            $scope.isLoading = true;
            Backups.getForVolume($scope.volumeId).then(function (data) {
                data.forEach(function (backup) {
                    backup.isSelected = false;
                });
                $scope.backups = data;
                $scope.isLoading = false;
            }, function () {
                $scope.isLoading = false;
            })
        };
        loadBackups();

        $scope.restore = function (backup) {
            $scope.objectToProcess = backup;
            var confirmInstance = $modal.open({
                animation: true,
                templateUrl: './partials/modal.history-restore.html',
                scope: $scope
            });

            confirmInstance.result.then(function () {
                var newTask = {
                    id: "",
                    priority: "",
                    volume: $scope.objectToProcess.volumeId,
                    backupFileName: $scope.objectToProcess.fileName,
                    type: "restore",
                    status: "waiting",
                    schedulerManual: true,
                    schedulerName: Storage.get('currentUser').email,
                    schedulerTime: Date.now()
                };
                Tasks.insert(newTask).then(function () {
                    var successInstance = $modal.open({
                        animation: true,
                        templateUrl: './partials/modal.task-created.html',
                        scope: $scope
                    });

                    successInstance.result.then(function () {
                        $state.go('app.tasks');
                    });
                });
            });

        };

    });