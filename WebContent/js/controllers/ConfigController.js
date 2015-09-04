'use strict';

angular.module('web')
    .controller('ConfigController', function ($scope, Volumes, Configuration, $modal, $state) {
        var DELAYTIME = 10000;
        var POSTREPEATS = 12;

        $scope.STRINGS = {
            s3: {
                new: 'Will be created new as',
                existing: ' bucket will be used'
            },
            db: {
                true: 'Database exists',
                false: 'No database found. You will be proposed to create a new user on the next step'
            },
            queue: {
                new: 'Will be created new as ',
                existing: ' queue will be used'
            },
            sdfs: {
                name: {
                    new: 'Will be created new as',
                    existing: ' name will be used'
                },
                size: {
                    new: 'Will be set as'
                },
                point: {
                    new: 'Will be at'
                }
            }
        };

        $scope.iconClass = {
            true: 'ok',
            false: 'minus'
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
            if (!$scope.settings.db.isValid) {
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

                    Configuration.send('current', newUser).then(

                    );

                    $scope.progressState = 'running';
                    var counter = 0;

                    function myTimeoutFunction() {
                        Volumes.get().then(function () {

                        }, function (data, status) {

                            if (status === 403) {
                                $scope.progressState = 'success';
                            }
                            else if (counter++ >= POSTREPEATS) {
                                $scope.progressState = 'failed';
                            } else {
                                setTimeout(myTimeoutFunction, DELAYTIME);
                            }
                        });
                    }

                    myTimeoutFunction();
                    wizardCreationProgress();

                });
            } else {
                Configuration.send('current').then();
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