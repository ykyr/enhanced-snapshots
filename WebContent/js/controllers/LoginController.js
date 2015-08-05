'use strict';

angular.module('web')
    .controller('LoginController', function ($scope, $state, Auth, Settings) {
        $scope.error = "";
        $scope.login = function () {
            Auth.logIn($scope.email, $scope.password).then(function () {
                if (Settings.get()) {
                    $state.go('app.volume.list');
                }
                else {
                    $state.go('aws');
                }
            }, function (res) {
                $scope.error = res;
                $scope.password = "";
            });
        }
    });