'use strict';

angular.module('web')
    .controller('ConfigController', function ($scope, Volumes, Configuration, $modal, $state) {
        var DELAYTIME = 600*1000;

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
                point:  'At mounting point:'
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

        var getAwsStatus = function () {
            Configuration.get('awscreds').then(function (result, status) {
                if (result.data.contains) {
                    getCurrentConfig();
                }
                else{
                    $scope.isAWS = true;
                }
            }, function (data, status) {
                $scope.isValidInstance = false;
                $scope.invalidMessage = data.data.localizedMessage;
            });
        };
        getAwsStatus();

        var getCurrentConfig = function () {
            Configuration.get('current').then(function (result, status) {
                $scope.settings = result.data;
                $scope.selectedBucket = (result.data.s3 || [])[0] || {};
                $scope.isAWS = false;
            }, function (data, status) {
                $scope.isValidInstance = false;
                $scope.invalidMessage = data.data.localizedMessage;
            })
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
            var settings = {
                bucketName: $scope.selectedBucket.bucketName
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

        var wizardCreationProgress = function () {
            var modalInstance = $modal.open({
                animation: true,
                templateUrl: './partials/modal.wizard-progress.html',
                scope: $scope
            });

            modalInstance.result.then(function () {
                $state.go('login')
            })
        };

    });