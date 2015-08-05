'use strict';

angular.module('web')
    .controller('HistoryController', function ($scope, Storage, ITEMS_BY_PAGE, DISPLAY_PAGES, $stateParams, $state, $modal, $filter, Backups, Tasks) {

        $scope.itemsByPage = ITEMS_BY_PAGE;
        $scope.displayedPages = DISPLAY_PAGES;

        $scope.volumeID = $stateParams.volumeID;

        $scope.backups = [];
        var loadBackups = function () {
            Backups.getForVolume($scope.volumeID).then(function (data) {
                $scope.backups = data;
            });
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
                    volume: $scope.backupToRestore.volume,
                    type: "restore",
                    status: "waiting",
                    schedulerManual: true,
                    schedulerName: Storage.get('currentUser').username, // TODO: Real user name should be used here
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