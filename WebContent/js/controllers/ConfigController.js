'use strict';

angular.module('web')
    .controller('ConfigController', function ($scope, Volumes, Configuration, $modal, $state) {
        var DELAYTIME = 600*1000;
            $scope.isBusy = false;
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
                size: 'Would you like to update volume size?'
            }
        };

        $scope.iconClass = {
            true: 'ok',
            false: 'cog'
        };

        $scope.statusColorClass = {
            true: 'success',
            false: 'danger'
        };

        $scope.isAWS = false;
        $scope.isValidInstance = true;
        $scope.selectBucket = function (bucket) {
            $scope.selectedBucket = bucket;
        };

        var wizardCreationProgress = function () {
            var modalInstance = $modal.open({
                animation: true,
                templateUrl: './partials/modal.wizard-progress.html',
                backdrop: false,
                scope: $scope
            });

            modalInstance.result.then(function () {
                $state.go('login')
            }, function () {
            });

            return modalInstance
        };

        var getAwsStatus = function () {
            $scope.progressState = 'loading';
            var loader = wizardCreationProgress();
            Configuration.get('awscreds').then(function (result, status) {
                if (result.data.contains) {
                    getCurrentConfig();
                    loader.dismiss();
                }
                else {
                    $scope.isAWS = true;
                    loader.dismiss();
                }
            }, function (data, status) {
                $scope.isValidInstance = false;
                $scope.invalidMessage = data.data.localizedMessage;
                loader.dismiss();
            });
        };
        getAwsStatus();

        var getCurrentConfig = function () {
            $scope.progressState = 'loading';
            var loader = wizardCreationProgress();

            Configuration.get('current').then(function (result, status) {
                $scope.settings = result.data;
                $scope.selectedBucket = (result.data.s3 || [])[0] || {};
                $scope.isAWS = false;
                loader.dismiss();
            }, function (data, status) {
                $scope.isValidInstance = false;
                $scope.invalidMessage = data.data.localizedMessage;
                loader.dismiss();
            });
        };

        $scope.awsPublicKey = '';
        $scope.awsSecretKey = '';

        $scope.sendAWS = function () {
            var keysCollection = {
                'awsPublicKey': $scope.awsPublicKey,
                'awsSecretKey': $scope.awsSecretKey
            };

            Configuration.send('awscreds', keysCollection).then(function () {
                getCurrentConfig();

            }, function (data, status) {
            });
        };

        $scope.sendSettings = function () {
            var volumeSize = $scope.isNewVolumeSize ? $scope.sdfsNewSize : $scope.settings.sdfs.volumeSize;

            var settings = {
                bucketName: $scope.selectedBucket.bucketName,
                volumeSize: volumeSize
            };

            if (!$scope.settings.db.hasAdmin) {
                $scope.userToEdit = {
                    isNew: true,
                    admin: true
                };

                var awsModalInstance = $modal.open({
                    animation: true,
                    templateUrl: './partials/modal.user-edit.html',
                    scope: $scope
                });

                awsModalInstance.result.then(function () {
                    settings.user = $scope.userToEdit;
                    delete settings.user.isNew;

                    $scope.progressState = 'running';
                    Configuration.send('current', settings, DELAYTIME).then(function () {
                        $scope.progressState = 'success';
                    }, function () {
                        $scope.progressState = 'failed';
                    });

                    wizardCreationProgress();

                });
            } else {
                $scope.progressState = 'running';

                Configuration.send('current', settings).then(function () {
                    $scope.progressState = 'success';
                }, function () {
                    $scope.progressState = 'failed';
                });

                wizardCreationProgress();
            }
        };


    });