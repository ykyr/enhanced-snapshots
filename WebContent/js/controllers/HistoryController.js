'use strict';

angular.module('web')
    .controller('HistoryController', function ($scope, Storage, ITEMS_BY_PAGE, DISPLAY_PAGES, $stateParams, $state, $modal, $filter, Backups, Tasks) {

        $scope.itemsByPage = ITEMS_BY_PAGE;
        $scope.displayedPages = DISPLAY_PAGES;

        $scope.volumeId = $stateParams.volumeId;

        $scope.isLoading = false;
        $scope.backups = [];
        var loadBackups = function () {
            $scope.isLoading = true;
            Backups.getForVolume($scope.volumeId).then(function (data) {
                $scope.backups = data;
                $scope.isLoading = false;
            }, function () {
                $scope.isLoading = false;
            })
        };
        loadBackups();

        $scope.restore = function (backup) {
            $scope.backupToRestore = backup;
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
                    backupFileName: $scope.backupToRestore.fileName,
                    type: "restore",
                    status: "waiting",
                    schedulerManual: true,
                    schedulerName: Storage.get('currentUser').username,
                    schedulerTime: $filter('date')(new Date(), "yyyy-MM-dd HH:mm:ss") // TODO: Move time format to global setting
                };
                Tasks.insert(newTask).then(function () {
                    var successInstance = $modal.open({
                        animation: true,
                        templateUrl: './partials/modal.task-restore-created.html'
                    });

                    successInstance.result.then(function () {
                        $state.go('app.tasks');
                    });
                });
            });

        };

        $scope.remove = function (backupFileName) {
            $scope.backupToDelete = backupFileName;

            var rejectInstance = $modal.open({
                animation: true,
                templateUrl: './partials/modal.backup-delete.html',
                scope: $scope
            });

            rejectInstance.result.then(function () {
                Backups.delete(backupFileName).then(function () {
                    loadBackups();
                });
            });
        };

    });