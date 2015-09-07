'use strict';

angular.module('web')
    .controller('ConfigController', function ($scope, Volumes, Configuration, $modal, $state) {
        var DELAYTIME = 120*1000;

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
            queue: {
                new: 'Will be created new as ',
                existing: 'Will be used existing queue:'
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

        $scope.isAWS = true;

        var getCurrentConfig = function () {
            Configuration.get('current').then(function (data, status) {
                $scope.settings = data.data;
                $scope.isAWS = false;
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
                $scope.error = data;
            });
        };

        $scope.sendSettings = function () {
            if (!$scope.settings.db.hasAdminUser) {
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
                    var newUser = $scope.userToEdit;
                    delete newUser.isNew;

                    $scope.progressState = 'running';
                    Configuration.send('current', newUser, DELAYTIME).then(function () {
                        $scope.progressState = 'success';
                    }, function () {
                        $scope.progressState = 'failed';
                    });

                    wizardCreationProgress();

                });
            } else {
                Configuration.send('current');
            }
        };

        var wizardCreationProgress = function () {
            var modalInstance = $modal.open({
                animation: true,
                templateUrl: './partials/modal.wizard-progress.html',
                scope: $scope
            });

            modalInstance.result.then(function () {
                $state.go('logout')
            })
        };

    });