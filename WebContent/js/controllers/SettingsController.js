'use strict';

angular.module('web')
    .controller('SettingsController', function ($scope, System, Users, $modal) {
        var currentUser = Users.getCurrent();
        $scope.isAdmin = currentUser.role === "admin";

        System.get().then(function (data) {
            $scope.settings = data;
            $scope.initialTempVolumeType = data.systemProperties.tempVolumeType;
            $scope.initialRestoreVolumeType = data.systemProperties.restoreVolumeType;
            $scope.initialTempVolumeSize = data.systemProperties.tempVolumeIopsPerGb;
            $scope.initialRestoreVolumeSize = data.systemProperties.restoreVolumeIopsPerGb;
        }, function (e) {
            console.log(e);
        });

        $scope.backup = function () {
            var modalScope = $scope.$new(true);
            $modal.open({
                animation: true,
                templateUrl: './partials/modal.system-backup.html',
                scope: modalScope,
                controller: 'modalSystemBackupCtrl'
            });
        };

        $scope.uninstall = function () {
            $modal.open({
                animation: true,
                templateUrl: './partials/modal.system-uninstall.html',
                controller: 'modalSystemUninstallCtrl'
            });

        };

        $scope.changeType = function () {
            var typeChangeConfirmationModal = $modal.open({
                animation: true,
                scope: $scope,
                templateUrl: './partials/modal.volume-type.html',
                controller: 'modalVolumeTypeChangeCtrl'
            });

            typeChangeConfirmationModal.result.then(function () {
                $scope.initialTempVolumeType = $scope.settings.systemProperties.tempVolumeType;
                $scope.initialRestoreVolumeType = $scope.settings.systemProperties.restoreVolumeType;
                $scope.initialTempVolumeSize = $scope.settings.systemProperties.tempVolumeIopsPerGb;
                $scope.initialRestoreVolumeSize = $scope.settings.systemProperties.restoreVolumeIopsPerGb;
            });
        };
    });