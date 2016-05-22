'use strict';

angular.module('web')
    .controller('SettingsController', function ($scope, System, Users, $modal) {
        var currentUser = Users.getCurrent();
        $scope.isAdmin = currentUser.role === "admin";

        $scope.STRINGS = {
            s3: {
                new: 'Will be created new as',
                existing: 'Will be used existing bucket:'
            },
            db: {
                isValid: {
                    true: 'Database exists',
                    false: 'No database found'
                },
                hasAdminUser: {
                    false: 'You will need to create a new user on the next step'
                }
            },
            sdfs: {
                name: {
                    new: 'Will be created new volume:',
                    existing: 'Will be used existing volume:'
                },
                point: 'At mounting point:',
                size: 'Would you like to update SDFS Settings?'
            }
        };

        var deleteIt = {
            sdfs: {
                volumeName: "awspool",
                volumeSize: 500,
                mountPoint: "/mnt/awspool/",
                minVolumeSize: 500,
                maxVolumeSize: 500,
                sdfsLocalCacheSize: 1,
                maxSdfsLocalCacheSize: 10,
                minSdfsLocalCacheSize: 1
            }
        };

        var deleteIt2 = {
            systemProperties: {
                restoreVolumeIopsPerGb: 30,
                restoreVolumeType: "gp2",
                tempVolumeIopsPerGb: 30,
                tempVolumeType: "gp2",
                volumeTypeOptions: ["gp2", "io1", "standard"],
                taskQueueSize: 20,
                amazonRetryCount: 20,
                amazonRetrySleep: 10
            }
        };


        System.get().then(function (data) {
            $scope.settings = data;
            $scope.initialSettings = data;
            //TODO remove it as we now have obj with all settings
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