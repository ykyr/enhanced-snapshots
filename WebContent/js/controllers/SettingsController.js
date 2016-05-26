'use strict';

angular.module('web')
    .controller('SettingsController', function ($scope, System, Users, $modal) {
        var currentUser = Users.getCurrent();
        $scope.isAdmin = currentUser.role === "admin";

        $scope.STRINGS = {
            s3: {
                empty: 'Bucket name field cannot be empty',
                existing: ' name is already in use. Please choose another one.'
            },
            sdfs: {
                sdfsLocalCacheSize: {
                  empty: 'Local Cache Size field cannot be empty.'
                } ,
                volumeSize: {
                 empty: 'Volume Size field cannot be empty.'
                }
            },
            volumeType: {
               empty: 'Volume size for io1 volume type cannot be empty.',
               range: 'Volume size for io1 volume type must be in between 1 and 30.'
            },
            otherSettings: {
                empty: 'All fields are required. Please fill in empty fields.'
            }
        };

        var progressLoader = function () {
            var modalInstance = $modal.open({
                animation: true,
                templateUrl: './partials/modal.wizard-progress.html',
                backdrop: false,
                scope: $scope
            });

            return modalInstance
        };

        $scope.progressState = 'loading';
        var loader = progressLoader();
        System.get().then(function (data) {
            $scope.settings = data;
            $scope.initialSettings = angular.copy(data);
            // $scope.s3MutableSuffix = $scope.initialSettings.s3.bucketName.slice($scope.initialSettings.s3.immutablePrefix.length);
            $scope.progressState = '';
            loader.dismiss();
        }, function (e) {
            console.log(e);
            $scope.progressState = 'failed';
            loader.dismiss();
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

        $scope.updateSettings = function () {

            var settingsUpdateModal = $modal.open({
                animation: true,
                scope: $scope,
                templateUrl: './partials/modal.settings-update.html',
                controller: 'modalVolumeTypeChangeCtrl'
            });

            settingsUpdateModal.result.then(function () {
                $scope.initialSettings = angular.copy($scope.settings);
            });
        };

        $scope.isNameExist = function (name) {
            // var initialName = $scope.initialSettings.s3.bucketName.slice($scope.initialSettings.s3.immutablePrefix.length);
          //   $scope.nameExists = $scope.initialSettings.s3.suffixesInUse.filter(function (suffix) {
          //     return suffix === name && name !== initialName;
          // }).length;
          //   $scope.s3.$setValidity("bucketName", false);
        };

        $scope.isNewValues = function () {
            if ($scope.settings) {
                // $scope.settings.s3.bucketName = $scope.settings.s3.immutablePrefix + $scope.s3MutableSuffix;

                if($scope.nameExists){
                    $scope.settings.s3.bucketName = $scope.initialSettings.s3.bucketName;
                }
            }
           return JSON.stringify($scope.settings) !== JSON.stringify($scope.initialSettings);
        };


    });